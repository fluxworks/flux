//! Rusqlite is an ergonomic wrapper for using SQLite from Rust.
#![feature
(

)]

#![allow
(
    non_camel_case_types,
    unused_attributes,
    unused_imports,
)]
/*

pub mod _
{
	pub use ::_::{ * };
}

pub mod __
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
#[macro_use] pub mod macros
{
	
	#[macro_export] macro_rules! params
	{
		() =>
		{
			&[] as &[&dyn ToSql]
		};
		
		($($param:expr),+ $(,)?) =>
		{
			&[$(&$param as &dyn ToSql),+] as &[&dyn ToSql]
		};
	}
	
	#[macro_export] macro_rules! named_params
	{
		() => {
			&[] as &[(&str, &dyn $crate::ToSql)]
		};
		($($param_name:literal: $param_val:expr),+ $(,)?) => {
			&[$(($param_name, &$param_val as &dyn ToSql)),+] as &[(&str, &dyn ToSql)]
		};
	}
	
	#[macro_export] macro_rules! err
	{
		($code:expr $(,)?) => {
			::sqlite3::error::error_from_sqlite_code($code, None)
		};
		($code:expr, $msg:literal $(,)?) => {
			::sqlite3::error::error_from_sqlite_code($code, Some(format!($msg)))
		};
		($code:expr, $err:expr $(,)?) => {
			::sqlite3::error::error_from_sqlite_code($code, Some(format!($err)))
		};
		($code:expr, $fmt:expr, $($arg:tt)*) => {
			::sqlite3::error::error_from_sqlite_code($code, Some(format!($fmt, $($arg)*)))
		};
	}

	#[macro_export] macro_rules! bitflags
	{
		(
			$(#[$outer:meta])*
			$vis:vis struct $BitFlags:ident: $T:ty {
				$(
					$(#[$inner:ident $($args:tt)*])*
					const $Flag:tt = $value:expr;
				)*
			}

			$($t:tt)*
		) => {
			// Declared in the scope of the `bitflags!` call
			// This type appears in the end-user's API
			$crate::__declare_public_bitflags! {
				$(#[$outer])*
				$vis struct $BitFlags
			}

			// Workaround for: https://github.com/bitflags/bitflags/issues/320
			$crate::__impl_public_bitflags_consts! {
				$BitFlags: $T {
					$(
						$(#[$inner $($args)*])*
						const $Flag = $value;
					)*
				}
			}

			#[allow(
				dead_code,
				deprecated,
				unused_doc_comments,
				unused_attributes,
				unused_mut,
				unused_imports,
				non_upper_case_globals,
				clippy::assign_op_pattern,
				clippy::indexing_slicing,
				clippy::same_name_method,
				clippy::iter_without_into_iter,
			)]
			const _: () = {
				// Declared in a "hidden" scope that can't be reached directly
				// These types don't appear in the end-user's API
				$crate::__declare_internal_bitflags! {
					$vis struct InternalBitFlags: $T
				}

				$crate::__impl_internal_bitflags! {
					InternalBitFlags: $T, $BitFlags {
						$(
							$(#[$inner $($args)*])*
							const $Flag = $value;
						)*
					}
				}

				// This is where new library trait implementations can be added
				$crate::__impl_external_bitflags! {
					InternalBitFlags: $T, $BitFlags {
						$(
							$(#[$inner $($args)*])*
							const $Flag;
						)*
					}
				}

				$crate::__impl_public_bitflags_forward! {
					$BitFlags: $T, InternalBitFlags
				}

				$crate::__impl_public_bitflags_ops! {
					$BitFlags
				}

				$crate::__impl_public_bitflags_iter! {
					$BitFlags: $T, $BitFlags
				}
			};

			$crate::bitflags! {
				$($t)*
			}
		};
		(
			$(#[$outer:meta])*
			impl $BitFlags:ident: $T:ty {
				$(
					$(#[$inner:ident $($args:tt)*])*
					const $Flag:tt = $value:expr;
				)*
			}

			$($t:tt)*
		) => {
			$crate::__impl_public_bitflags_consts! {
				$BitFlags: $T {
					$(
						$(#[$inner $($args)*])*
						const $Flag = $value;
					)*
				}
			}

			#[allow(
				dead_code,
				deprecated,
				unused_doc_comments,
				unused_attributes,
				unused_mut,
				unused_imports,
				non_upper_case_globals,
				clippy::assign_op_pattern,
				clippy::iter_without_into_iter,
			)]
			const _: () = {
				$crate::__impl_public_bitflags! {
					$(#[$outer])*
					$BitFlags: $T, $BitFlags {
						$(
							$(#[$inner $($args)*])*
							const $Flag = $value;
						)*
					}
				}

				$crate::__impl_public_bitflags_ops! {
					$BitFlags
				}

				$crate::__impl_public_bitflags_iter! {
					$BitFlags: $T, $BitFlags
				}
			};

			$crate::bitflags! {
				$($t)*
			}
		};
		() => {};
	}
	/// Implement functions on bitflags types.
	///
	/// We need to be careful about adding new methods and trait implementations here because they
	/// could conflict with items added by the end-user.
	#[macro_export]
	#[doc(hidden)]
	macro_rules! __impl_bitflags {
		(
			$(#[$outer:meta])*
			$PublicBitFlags:ident: $T:ty {
				fn empty() $empty:block
				fn all() $all:block
				fn bits($bits0:ident) $bits:block
				fn from_bits($from_bits0:ident) $from_bits:block
				fn from_bits_truncate($from_bits_truncate0:ident) $from_bits_truncate:block
				fn from_bits_retain($from_bits_retain0:ident) $from_bits_retain:block
				fn from_name($from_name0:ident) $from_name:block
				fn is_empty($is_empty0:ident) $is_empty:block
				fn is_all($is_all0:ident) $is_all:block
				fn intersects($intersects0:ident, $intersects1:ident) $intersects:block
				fn contains($contains0:ident, $contains1:ident) $contains:block
				fn insert($insert0:ident, $insert1:ident) $insert:block
				fn remove($remove0:ident, $remove1:ident) $remove:block
				fn toggle($toggle0:ident, $toggle1:ident) $toggle:block
				fn set($set0:ident, $set1:ident, $set2:ident) $set:block
				fn intersection($intersection0:ident, $intersection1:ident) $intersection:block
				fn union($union0:ident, $union1:ident) $union:block
				fn difference($difference0:ident, $difference1:ident) $difference:block
				fn symmetric_difference($symmetric_difference0:ident, $symmetric_difference1:ident) $symmetric_difference:block
				fn complement($complement0:ident) $complement:block
			}
		) => {
			#[allow(dead_code, deprecated, unused_attributes)]
			$(#[$outer])*
			impl $PublicBitFlags {
				/// Get a flags value with all bits unset.
				#[inline]
				pub const fn empty() -> Self {
					$empty
				}

				/// Get a flags value with all known bits set.
				#[inline]
				pub const fn all() -> Self {
					$all
				}

				/// Get the underlying bits value.
				///
				/// The returned value is exactly the bits set in this flags value.
				#[inline]
				pub const fn bits(&self) -> $T {
					let $bits0 = self;
					$bits
				}

				/// Convert from a bits value.
				///
				/// This method will return `None` if any unknown bits are set.
				#[inline]
				pub const fn from_bits(bits: $T) -> $crate::__private::core::option::Option<Self> {
					let $from_bits0 = bits;
					$from_bits
				}

				/// Convert from a bits value, unsetting any unknown bits.
				#[inline]
				pub const fn from_bits_truncate(bits: $T) -> Self {
					let $from_bits_truncate0 = bits;
					$from_bits_truncate
				}

				/// Convert from a bits value exactly.
				#[inline]
				pub const fn from_bits_retain(bits: $T) -> Self {
					let $from_bits_retain0 = bits;
					$from_bits_retain
				}

				/// Get a flags value with the bits of a flag with the given name set.
				///
				/// This method will return `None` if `name` is empty or doesn't
				/// correspond to any named flag.
				#[inline]
				pub fn from_name(name: &str) -> $crate::__private::core::option::Option<Self> {
					let $from_name0 = name;
					$from_name
				}

				/// Whether all bits in this flags value are unset.
				#[inline]
				pub const fn is_empty(&self) -> bool {
					let $is_empty0 = self;
					$is_empty
				}

				/// Whether all known bits in this flags value are set.
				#[inline]
				pub const fn is_all(&self) -> bool {
					let $is_all0 = self;
					$is_all
				}

				/// Whether any set bits in a source flags value are also set in a target flags value.
				#[inline]
				pub const fn intersects(&self, other: Self) -> bool {
					let $intersects0 = self;
					let $intersects1 = other;
					$intersects
				}

				/// Whether all set bits in a source flags value are also set in a target flags value.
				#[inline]
				pub const fn contains(&self, other: Self) -> bool {
					let $contains0 = self;
					let $contains1 = other;
					$contains
				}

				/// The bitwise or (`|`) of the bits in two flags values.
				#[inline]
				pub fn insert(&mut self, other: Self) {
					let $insert0 = self;
					let $insert1 = other;
					$insert
				}

				/// The intersection of a source flags value with the complement of a target flags value (`&!`).
				///
				/// This method is not equivalent to `self & !other` when `other` has unknown bits set.
				/// `remove` won't truncate `other`, but the `!` operator will.
				#[inline]
				pub fn remove(&mut self, other: Self) {
					let $remove0 = self;
					let $remove1 = other;
					$remove
				}

				/// The bitwise exclusive-or (`^`) of the bits in two flags values.
				#[inline]
				pub fn toggle(&mut self, other: Self) {
					let $toggle0 = self;
					let $toggle1 = other;
					$toggle
				}

				/// Call `insert` when `value` is `true` or `remove` when `value` is `false`.
				#[inline]
				pub fn set(&mut self, other: Self, value: bool) {
					let $set0 = self;
					let $set1 = other;
					let $set2 = value;
					$set
				}

				/// The bitwise and (`&`) of the bits in two flags values.
				#[inline]
				#[must_use]
				pub const fn intersection(self, other: Self) -> Self {
					let $intersection0 = self;
					let $intersection1 = other;
					$intersection
				}

				/// The bitwise or (`|`) of the bits in two flags values.
				#[inline]
				#[must_use]
				pub const fn union(self, other: Self) -> Self {
					let $union0 = self;
					let $union1 = other;
					$union
				}

				/// The intersection of a source flags value with the complement of a target flags value (`&!`).
				///
				/// This method is not equivalent to `self & !other` when `other` has unknown bits set.
				/// `difference` won't truncate `other`, but the `!` operator will.
				#[inline]
				#[must_use]
				pub const fn difference(self, other: Self) -> Self {
					let $difference0 = self;
					let $difference1 = other;
					$difference
				}

				/// The bitwise exclusive-or (`^`) of the bits in two flags values.
				#[inline]
				#[must_use]
				pub const fn symmetric_difference(self, other: Self) -> Self {
					let $symmetric_difference0 = self;
					let $symmetric_difference1 = other;
					$symmetric_difference
				}

				/// The bitwise negation (`!`) of the bits in a flags value, truncating the result.
				#[inline]
				#[must_use]
				pub const fn complement(self) -> Self {
					let $complement0 = self;
					$complement
				}
			}
		};
	}
	/// A macro that processed the input to `bitflags!` and shuffles attributes around
	/// based on whether or not they're "expression-safe".
	///
	/// This macro is a token-tree muncher that works on 2 levels:
	///
	/// For each attribute, we explicitly match on its identifier, like `cfg` to determine
	/// whether or not it should be considered expression-safe.
	///
	/// If you find yourself with an attribute that should be considered expression-safe
	/// and isn't, it can be added here.
	#[macro_export]
	#[doc(hidden)]
	macro_rules! __bitflags_expr_safe_attrs {
		// Entrypoint: Move all flags and all attributes into `unprocessed` lists
		// where they'll be munched one-at-a-time
		(
			$(#[$inner:ident $($args:tt)*])*
			{ $e:expr }
		) => {
			$crate::__bitflags_expr_safe_attrs! {
				expr: { $e },
				attrs: {
					// All attributes start here
					unprocessed: [$(#[$inner $($args)*])*],
					// Attributes that are safe on expressions go here
					processed: [],
				},
			}
		};
		// Process the next attribute on the current flag
		// `cfg`: The next flag should be propagated to expressions
		// NOTE: You can copy this rules block and replace `cfg` with
		// your attribute name that should be considered expression-safe
		(
			expr: { $e:expr },
				attrs: {
				unprocessed: [
					// cfg matched here
					#[cfg $($args:tt)*]
					$($attrs_rest:tt)*
				],
				processed: [$($expr:tt)*],
			},
		) => {
			$crate::__bitflags_expr_safe_attrs! {
				expr: { $e },
				attrs: {
					unprocessed: [
						$($attrs_rest)*
					],
					processed: [
						$($expr)*
						// cfg added here
						#[cfg $($args)*]
					],
				},
			}
		};
		// Process the next attribute on the current flag
		// `$other`: The next flag should not be propagated to expressions
		(
			expr: { $e:expr },
				attrs: {
				unprocessed: [
					// $other matched here
					#[$other:ident $($args:tt)*]
					$($attrs_rest:tt)*
				],
				processed: [$($expr:tt)*],
			},
		) => {
			$crate::__bitflags_expr_safe_attrs! {
				expr: { $e },
					attrs: {
					unprocessed: [
						$($attrs_rest)*
					],
					processed: [
						// $other not added here
						$($expr)*
					],
				},
			}
		};
		// Once all attributes on all flags are processed, generate the actual code
		(
			expr: { $e:expr },
			attrs: {
				unprocessed: [],
				processed: [$(#[$expr:ident $($exprargs:tt)*])*],
			},
		) => {
			$(#[$expr $($exprargs)*])*
			{ $e }
		}
	}
	/// Implement a flag, which may be a wildcard `_`.
	#[macro_export]
	#[doc(hidden)]
	macro_rules! __bitflags_flag {
		(
			{
				name: _,
				named: { $($named:tt)* },
				unnamed: { $($unnamed:tt)* },
			}
		) => {
			$($unnamed)*
		};
		(
			{
				name: $Flag:ident,
				named: { $($named:tt)* },
				unnamed: { $($unnamed:tt)* },
			}
		) => {
			$($named)*
		};
	}
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

pub mod default
{
	pub use std::default::{ * };
}

pub mod error
{
	pub use std::error::{ * };
}

pub mod fallible
{
	/*!
	*/
	use ::
	{
		*,
	};
	/*
	*/
	pub mod iterator
	{
		/*!
		"Fallible" iterators. */
		use ::
		{
			*,
		};
		/*
		#![doc(html_root_url = "https://docs.rs/fallible-iterator/0.2")]
		#![warn(missing_docs)]
		#![no_std]

		use core::cmp::{self, Ordering};
		use core::convert::Infallible;
		use core::iter;
		use core::marker::PhantomData;

		#[cfg(feature = "alloc")]
		extern crate alloc;

		#[cfg(feature = "alloc")]
		use alloc::boxed::Box;

		#[cfg(all(test, feature = "alloc"))]
		mod test;
		*/	
		pub mod streams
		{
			/*!
			fallible-streaming-iterator v0.1.0
			Fallible, streaming iteration. */
			use ::
			{
				*,
			};
			/*
			*/
			/// A fallible, streaming iterator.
			pub trait FallibleStreamingIterator
			{
				/// The type being iterated over.
				type Item: ?Sized;

				/// The error type of iteration.
				type Error;

				/// Advances the iterator to the next position.
				///
				/// Iterators start just before the first item, so this method should be called before `get`
				/// when iterating.
				///
				/// The behavior of calling this method after `get` has returned `None`, or after this method
				/// has returned an error is unspecified.
				fn advance(&mut self) -> Result<(), Self::Error>;

				/// Returns the current element.
				///
				/// The behavior of calling this method before any calls to `advance` is unspecified.
				fn get(&self) -> Option<&Self::Item>;

				/// Advances the iterator, returning the next element.
				///
				/// The default implementation simply calls `advance` followed by `get`.
				#[inline]
				fn next(&mut self) -> Result<Option<&Self::Item>, Self::Error> {
					self.advance()?;
					Ok(self.get())
				}

				/// Returns bounds on the number of remaining elements in the iterator.
				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					(0, None)
				}

				/// Determines if all elements of the iterator satisfy a predicate.
				#[inline]
				fn all<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					while let Some(e) = self.next()? {
						if !f(e) {
							return Ok(false);
						}
					}
					Ok(true)
				}

				/// Determines if any elements of the iterator satisfy a predicate.
				#[inline]
				fn any<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					self.all(|e| !f(e)).map(|r| !r)
				}

				/// Borrows an iterator, rather than consuming it.
				///
				/// This is useful to allow the application of iterator adaptors while still retaining ownership
				/// of the original adaptor.
				#[inline]
				fn by_ref(&mut self) -> &mut Self
					where Self: Sized
				{
					self
				}

				/// Returns the number of remaining elements in the iterator.
				#[inline]
				fn count(mut self) -> Result<usize, Self::Error>
					where Self: Sized
				{
					let mut count = 0;
					while let Some(_) = self.next()? {
						count += 1;
					}
					Ok(count)
				}

				/// Returns an iterator which filters elements by a predicate.
				#[inline]
				fn filter<F>(self, f: F) -> Filter<Self, F>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					Filter {
						it: self,
						f: f,
					}
				}

				/// Returns the first element of the iterator which satisfies a predicate.
				#[inline]
				fn find<F>(&mut self, mut f: F) -> Result<Option<&Self::Item>, Self::Error>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					loop {
						self.advance()?;
						match self.get() {
							Some(v) => {
								if f(v) {
									break;
								}
							}
							None => break,
						}
					}
					Ok((*self).get())
				}

				/// Returns an iterator which is well-behaved at the beginning and end of iteration.
				#[inline]
				fn fuse(self) -> Fuse<Self>
					where Self: Sized
				{
					Fuse {
						it: self,
						state: FuseState::Start,
					}
				}

				/// Returns an iterator which applies a transform to elements.
				#[inline]
				fn map<F, B>(self, f: F) -> Map<Self, F, B>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> B
				{
					Map {
						it: self,
						f: f,
						value: None,
					}
				}

				/// Returns an iterator which applies a transform to elements.
				///
				/// Unlike `map`, the the closure provided to this method returns a reference into the original
				/// value.
				#[inline]
				fn map_ref<F, B: ?Sized>(self, f: F) -> MapRef<Self, F>
					where Self: Sized,
						  F: Fn(&Self::Item) -> &B
				{
					MapRef {
						it: self,
						f: f,
					}
				}

				/// Returns the `nth` element of the iterator.
				#[inline]
				fn nth(&mut self, n: usize) -> Result<Option<&Self::Item>, Self::Error> {
					for _ in 0..n {
						self.advance()?;
						if let None = self.get() {
							return Ok(None);
						}
					}
					self.next()
				}

				/// Returns the position of the first element matching a predicate.
				#[inline]
				fn position<F>(&mut self, mut f: F) -> Result<Option<usize>, Self::Error>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					let mut pos = 0;
					while let Some(v) = self.next()? {
						if f(v) {
							return Ok(Some(pos));
						}
						pos += 1;
					}
					Ok(None)
				}

				/// Returns an iterator which skips the first `n` elements.
				#[inline]
				fn skip(self, n: usize) -> Skip<Self>
					where Self: Sized
				{
					Skip {
						it: self,
						n: n,
					}
				}

				/// Returns an iterator which skips the first sequence of elements matching a predicate.
				#[inline]
				fn skip_while<F>(self, f: F) -> SkipWhile<Self, F>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					SkipWhile {
						it: self,
						f: f,
						done: false,
					}
				}

				/// Returns an iterator which only returns the first `n` elements.
				#[inline]
				fn take(self, n: usize) -> Take<Self>
					where Self: Sized
				{
					Take {
						it: self,
						n: n,
						done: false,
					}
				}

				/// Returns an iterator which only returns the first sequence of elements matching a predicate.
				#[inline]
				fn take_while<F>(self, f: F) -> TakeWhile<Self, F>
					where Self: Sized,
						  F: FnMut(&Self::Item) -> bool
				{
					TakeWhile {
						it: self,
						f: f,
						done: false,
					}
				}
			}
			/// An iterator which filters elements with a predicate.
			pub struct Filter<I, F>
			{
				it: I,
				f: F,
			}

			impl<I, F> FallibleStreamingIterator for Filter<I, F> where
			I: FallibleStreamingIterator,
			F: FnMut(&I::Item) -> bool
			{
				type Item = I::Item;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					while let Some(i) = self.it.next()? {
						if (self.f)(i) {
							break;
						}
					}
					Ok(())
				}

				#[inline]
				fn get(&self) -> Option<&I::Item> {
					self.it.get()
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					(0, self.it.size_hint().1)
				}
			}

			#[derive(Copy, Clone)]
			enum FuseState
			{
				Start,
				Middle,
				End,
			}
			/// An iterator which is well-behaved at the beginning and end of iteration.
			pub struct Fuse<I>
			{
				it: I,
				state: FuseState,
			}

			impl<I> FallibleStreamingIterator for Fuse<I> where I: FallibleStreamingIterator
			{
				type Item = I::Item;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					match self.state {
						FuseState::Start => {
							match self.it.next() {
								Ok(Some(_)) => self.state = FuseState::Middle,
								Ok(None) => self.state = FuseState::End,
								Err(e) => {
									self.state = FuseState::End;
									return Err(e)
								}
							};
						}
						FuseState::Middle => {
							match self.it.next() {
								Ok(Some(_)) => {}
								Ok(None) => self.state = FuseState::End,
								Err(e) => {
									self.state = FuseState::End;
									return Err(e)
								}
							}
						}
						FuseState::End => {},
					}
					Ok(())
				}

				#[inline]
				fn get(&self) -> Option<&I::Item> {
					match self.state {
						FuseState::Middle => self.it.get(),
						FuseState::Start | FuseState::End => None,
					}
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					self.it.size_hint()
				}

				#[inline]
				fn next(&mut self) -> Result<Option<&I::Item>, I::Error> {
					match self.state {
						FuseState::Start => {
							match self.it.next() {
								Ok(Some(v)) => {
									self.state = FuseState::Middle;
									Ok(Some(v))
								}
								Ok(None) => {
									self.state = FuseState::End;
									Ok(None)
								}
								Err(e) => {
									self.state = FuseState::End;
									Err(e)
								}
							}
						}
						FuseState::Middle => {
							match self.it.next() {
								Ok(Some(v)) => Ok(Some(v)),
								Ok(None) => {
									self.state = FuseState::End;
									Ok(None)
								}
								Err(e) => {
									self.state = FuseState::End;
									Err(e)
								}
							}
						}
						FuseState::End => Ok(None)
					}
				}
			}
			/// An iterator which applies a transform to elements.
			pub struct Map<I, F, B>
			{
				it: I,
				f: F,
				value: Option<B>,
			}

			impl<I, F, B> FallibleStreamingIterator for Map<I, F, B> where
			I: FallibleStreamingIterator,
			F: FnMut(&I::Item) -> B
			{
				type Item = B;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					self.value = self.it.next()?.map(&mut self.f);
					Ok(())
				}

				#[inline]
				fn get(&self) -> Option<&B> {
					self.value.as_ref()
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					self.it.size_hint()
				}
			}
			/// An iterator which applies a transform to elements.
			pub struct MapRef<I, F>
			{
				it: I,
				f: F,
			}

			impl<I, F, B: ?Sized> FallibleStreamingIterator for MapRef<I, F> where
			I: FallibleStreamingIterator,
			F: Fn(&I::Item) -> &B,
			{
				type Item = B;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					self.it.advance()
				}

				#[inline]
				fn get(&self) -> Option<&B> {
					self.it.get().map(&self.f)
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					self.it.size_hint()
				}
			}
			/// Returns an iterator which skips a number of initial elements.
			pub struct Skip<I>
			{
				it: I,
				n: usize,
			}

			impl<I> FallibleStreamingIterator for Skip<I> where
			I: FallibleStreamingIterator
			{
				type Item = I::Item;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					for _ in 0..self.n {
						if let None = self.it.next()? {
							return Ok(());
						}
					}
					self.n = 0;
					self.advance()
				}

				#[inline]
				fn get(&self) -> Option<&I::Item> {
					self.it.get()
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					let hint = self.it.size_hint();
					(hint.0.saturating_sub(self.n), hint.1.map(|h| h.saturating_sub(self.n)))
				}
			}
			/// An iterator which skips initial elements matching a predicate.
			pub struct SkipWhile<I, F>
			{
				it: I,
				f: F,
				done: bool,
			}

			impl<I, F> FallibleStreamingIterator for SkipWhile<I, F> where
			I: FallibleStreamingIterator,
			F: FnMut(&I::Item) -> bool
			{
				type Item = I::Item;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					if !self.done {
						self.done = true;
						let f = &mut self.f;
						self.it.find(|i| !f(i)).map(|_| ())
					} else {
						self.it.advance()
					}
				}

				#[inline]
				fn get(&self) -> Option<&I::Item> {
					self.it.get()
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					let hint = self.it.size_hint();
					if self.done {
						hint
					} else {
						(0, hint.1)
					}
				}
			}
			/// An iterator which only returns a number of initial elements.
			pub struct Take<I>
			{
				it: I,
				n: usize,
				done: bool,
			}

			impl<I> FallibleStreamingIterator for Take<I> where
			I: FallibleStreamingIterator
			{
				type Item = I::Item;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					if self.n != 0 {
						self.it.advance()?;
						self.n -= 1;
					} else {
						self.done = true;
					}
					Ok(())
				}

				#[inline]
				fn get(&self) -> Option<&I::Item> {
					if self.done { self.it.get() } else { None }
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					if self.done {
						(0, Some(0))
					} else {
						let hint = self.it.size_hint();
						(hint.0.saturating_sub(self.n), hint.1.map(|h| h.saturating_sub(self.n)))
					}
				}
			}
			/// An iterator which only returns initial elements matching a predicate.
			pub struct TakeWhile<I, F>
			{
				it: I,
				f: F,
				done: bool,
			}

			impl<I, F> FallibleStreamingIterator for TakeWhile<I, F> where
			I: FallibleStreamingIterator,
			F: FnMut(&I::Item) -> bool
			{
				type Item = I::Item;
				type Error = I::Error;

				#[inline]
				fn advance(&mut self) -> Result<(), I::Error> {
					if let Some(v) = self.it.next()? {
						if !(self.f)(v) {
							self.done = true;
						}
					}
					Ok(())
				}

				#[inline]
				fn get(&self) -> Option<&I::Item> {
					if self.done { None } else { self.it.get() }
				}

				#[inline]
				fn size_hint(&self) -> (usize, Option<usize>) {
					if self.done {
						(0, Some(0))
					} else {
						(0, self.it.size_hint().1)
					}
				}
			}
		}

		enum FoldStop<T, E>
		{
			Break(T),
			Err(E),
		}

		impl<T, E> From<E> for FoldStop<T, E> {
			#[inline] fn from(e: E) -> FoldStop<T, E> {
				FoldStop::Err(e)
			}
		}

		trait ResultExt<T, E>
        {
			fn unpack_fold(self) -> Result<T, E>;
		}

		impl<T, E> ResultExt<T, E> for Result<T, FoldStop<T, E>> {
			#[inline] fn unpack_fold(self) -> Result<T, E> {
				match self {
					Ok(v) => Ok(v),
					Err(FoldStop::Break(v)) => Ok(v),
					Err(FoldStop::Err(e)) => Err(e),
				}
			}
		}
		/// An `Iterator`-like trait that allows for calculation of items to fail.
		pub trait FallibleIterator {
			/// The type being iterated over.
			type Item;

			/// The error type.
			type Error;

			/// Advances the iterator and returns the next value.
			///
			/// Returns `Ok(None)` when iteration is finished.
			///
			/// The behavior of calling this method after a previous call has returned
			/// `Ok(None)` or `Err` is implementation defined.
			fn next(&mut self) -> Result<Option<Self::Item>, Self::Error>;

			/// Returns bounds on the remaining length of the iterator.
			///
			/// Specifically, the first half of the returned tuple is a lower bound and
			/// the second half is an upper bound.
			///
			/// For the upper bound, `None` indicates that the upper bound is either
			/// unknown or larger than can be represented as a `usize`.
			///
			/// Both bounds assume that all remaining calls to `next` succeed. That is,
			/// `next` could return an `Err` in fewer calls than specified by the lower
			/// bound.
			///
			/// The default implementation returns `(0, None)`, which is correct for
			/// any iterator.
			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(0, None)
			}
			/// Consumes the iterator, returning the number of remaining items.
			#[inline] fn count(self) -> Result<usize, Self::Error>
			where
				Self: Sized,
			{
				self.fold(0, |n, _| Ok(n + 1))
			}

			#[inline]
			/// Sums the iterator elements.
			fn sum<I>(self) -> Result<I, Self::Error>
			where
				Self: Sized,
				I: iter::Sum<Self::Item>,
			{
				iter::Sum::sum(self.iterator())
			}

			#[inline]
			/// Returns the iterator elements product.
			fn product<I>(self) -> Result<I, Self::Error>
			where
				Self: Sized,
				I: iter::Product<Self::Item>,
			{
				iter::Product::product(self.iterator())
			}
			/// Returns the last element of the iterator.
			#[inline] fn last(self) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
			{
				self.fold(None, |_, v| Ok(Some(v)))
			}
			/// Returns the `n`th element of the iterator.
			#[inline] fn nth(&mut self, mut n: usize) -> Result<Option<Self::Item>, Self::Error> {
				while let Some(e) = self.next()? {
					if n == 0 {
						return Ok(Some(e));
					}
					n -= 1;
				}
				Ok(None)
			}
			/// Returns an iterator starting at the same point, but stepping by the given amount at each iteration.
			///
			/// # Panics
			///
			/// Panics if `step` is 0.
			#[inline] fn step_by(self, step: usize) -> StepBy<Self>
			where
				Self: Sized,
			{
				assert!(step != 0);
				StepBy {
					it: self,
					step: step - 1,
					first_take: true,
				}
			}
			/// Returns an iterator which yields the elements of this iterator followed
			/// by another.
			#[inline] fn chain<I>(self, it: I) -> Chain<Self, I>
			where
				I: IntoFallibleIterator<Item = Self::Item, Error = Self::Error>,
				Self: Sized,
			{
				Chain {
					front: self,
					back: it,
					state: ChainState::Both,
				}
			}
			/// Returns an iterator that yields pairs of this iterator's and another
			/// iterator's values.
			#[inline] fn zip<I>(self, o: I) -> Zip<Self, I::IntoFallibleIter>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
			{
				Zip(self, o.into_fallible_iter())
			}
			/// Returns an iterator which applies a fallible transform to the elements
			/// of the underlying iterator.
			#[inline] fn map<F, B>(self, f: F) -> Map<Self, F>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<B, Self::Error>,
			{
				Map { it: self, f }
			}
			/// Calls a fallible closure on each element of an iterator.
			#[inline] fn for_each<F>(self, mut f: F) -> Result<(), Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<(), Self::Error>,
			{
				self.fold((), move |(), item| f(item))
			}
			/// Returns an iterator which uses a predicate to determine which values
			/// should be yielded. The predicate may fail; such failures are passed to
			/// the caller.
			#[inline] fn filter<F>(self, f: F) -> Filter<Self, F>
			where
				Self: Sized,
				F: FnMut(&Self::Item) -> Result<bool, Self::Error>,
			{
				Filter { it: self, f }
			}
			/// Returns an iterator which both filters and maps. The closure may fail;
			/// such failures are passed along to the consumer.
			#[inline] fn filter_map<B, F>(self, f: F) -> FilterMap<Self, F>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<Option<B>, Self::Error>,
			{
				FilterMap { it: self, f }
			}
			/// Returns an iterator which yields the current iteration count as well
			/// as the value.
			#[inline] fn enumerate(self) -> Enumerate<Self>
			where
				Self: Sized,
			{
				Enumerate { it: self, n: 0 }
			}
			/// Returns an iterator that can peek at the next element without consuming
			/// it.
			#[inline] fn peekable(self) -> Peekable<Self>
			where
				Self: Sized,
			{
				Peekable {
					it: self,
					next: None,
				}
			}
			/// Returns an iterator that skips elements based on a predicate.
			#[inline] fn skip_while<P>(self, predicate: P) -> SkipWhile<Self, P>
			where
				Self: Sized,
				P: FnMut(&Self::Item) -> Result<bool, Self::Error>,
			{
				SkipWhile {
					it: self,
					flag: false,
					predicate,
				}
			}
			/// Returns an iterator that yields elements based on a predicate.
			#[inline] fn take_while<P>(self, predicate: P) -> TakeWhile<Self, P>
			where
				Self: Sized,
				P: FnMut(&Self::Item) -> Result<bool, Self::Error>,
			{
				TakeWhile {
					it: self,
					flag: false,
					predicate,
				}
			}
			/// Returns an iterator which skips the first `n` values of this iterator.
			#[inline] fn skip(self, n: usize) -> Skip<Self>
			where
				Self: Sized,
			{
				Skip { it: self, n }
			}
			/// Returns an iterator that yields only the first `n` values of this
			/// iterator.
			#[inline] fn take(self, n: usize) -> Take<Self>
			where
				Self: Sized,
			{
				Take {
					it: self,
					remaining: n,
				}
			}
			/// Returns an iterator which applies a stateful map to values of this
			/// iterator.
			#[inline] fn scan<St, B, F>(self, initial_state: St, f: F) -> Scan<Self, St, F>
			where
				Self: Sized,
				F: FnMut(&mut St, Self::Item) -> Result<Option<B>, Self::Error>,
			{
				Scan {
					it: self,
					f,
					state: initial_state,
				}
			}
			/// Returns an iterator which maps this iterator's elements to iterators, yielding those iterators' values.
			#[inline] fn flat_map<U, F>(self, f: F) -> FlatMap<Self, U, F>
			where
				Self: Sized,
				U: IntoFallibleIterator<Error = Self::Error>,
				F: FnMut(Self::Item) -> Result<U, Self::Error>,
			{
				FlatMap {
					it: self.map(f),
					cur: None,
				}
			}
			/// Returns an iterator which flattens an iterator of iterators, yielding those iterators' values.
			#[inline] fn flatten(self) -> Flatten<Self>
			where
				Self: Sized,
				Self::Item: IntoFallibleIterator<Error = Self::Error>,
			{
				Flatten {
					it: self,
					cur: None,
				}
			}
			/// Returns an iterator which yields this iterator's elements and ends after
			/// the first `Ok(None)`.
			///
			/// The behavior of calling `next` after it has previously returned
			/// `Ok(None)` is normally unspecified. The iterator returned by this method
			/// guarantees that `Ok(None)` will always be returned.
			#[inline] fn fuse(self) -> Fuse<Self>
			where
				Self: Sized,
			{
				Fuse {
					it: self,
					done: false,
				}
			}
			/// Returns an iterator which passes each element to a closure before returning it.
			#[inline] fn inspect<F>(self, f: F) -> Inspect<Self, F>
			where
				Self: Sized,
				F: FnMut(&Self::Item) -> Result<(), Self::Error>,
			{
				Inspect { it: self, f }
			}
			/// Borrow an iterator rather than consuming it.
			///
			/// This is useful to allow the use of iterator adaptors that would
			/// otherwise consume the value.
			#[inline] fn by_ref(&mut self) -> &mut Self
			where
				Self: Sized,
			{
				self
			}
			/// Transforms the iterator into a collection.
			///
			/// An `Err` will be returned if any invocation of `next` returns `Err`.
			#[inline] fn collect<T>(self) -> Result<T, Self::Error>
			where
				T: iter::FromIterator<Self::Item>,
				Self: Sized,
			{
				self.iterator().collect()
			}
			/// Transforms the iterator into two collections, partitioning elements by a closure.
			#[inline] fn partition<B, F>(self, mut f: F) -> Result<(B, B), Self::Error>
			where
				Self: Sized,
				B: Default + Extend<Self::Item>,
				F: FnMut(&Self::Item) -> Result<bool, Self::Error>,
			{
				let mut a = B::default();
				let mut b = B::default();

				self.for_each(|i| {
					if f(&i)? {
						a.extend(Some(i));
					} else {
						b.extend(Some(i));
					}
					Ok(())
				})?;

				Ok((a, b))
			}
			/// Applies a function over the elements of the iterator, producing a single
			/// final value.
			#[inline] fn fold<B, F>(mut self, init: B, f: F) -> Result<B, Self::Error>
			where
				Self: Sized,
				F: FnMut(B, Self::Item) -> Result<B, Self::Error>,
			{
				self.try_fold(init, f)
			}
			/// Applies a function over the elements of the iterator, producing a single final value.
			///
			/// This is used as the "base" of many methods on `FallibleIterator`.
			#[inline] fn try_fold<B, E, F>(&mut self, mut init: B, mut f: F) -> Result<B, E>
			where
				Self: Sized,
				E: From<Self::Error>,
				F: FnMut(B, Self::Item) -> Result<B, E>,
			{
				while let Some(v) = self.next()? {
					init = f(init, v)?;
				}
				Ok(init)
			}
			/// Determines if all elements of this iterator match a predicate.
			#[inline] fn all<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<bool, Self::Error>,
			{
				self.try_fold((), |(), v| {
					if !f(v)? {
						return Err(FoldStop::Break(false));
					}
					Ok(())
				})
				.map(|()| true)
				.unpack_fold()
			}
			/// Determines if any element of this iterator matches a predicate.
			#[inline] fn any<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<bool, Self::Error>,
			{
				self.try_fold((), |(), v| {
					if f(v)? {
						return Err(FoldStop::Break(true));
					}
					Ok(())
				})
				.map(|()| false)
				.unpack_fold()
			}
			/// Returns the first element of the iterator that matches a predicate.
			#[inline] fn find<F>(&mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				F: FnMut(&Self::Item) -> Result<bool, Self::Error>,
			{
				self.try_fold((), |(), v| {
					if f(&v)? {
						return Err(FoldStop::Break(Some(v)));
					}
					Ok(())
				})
				.map(|()| None)
				.unpack_fold()
			}
			/// Applies a function to the elements of the iterator, returning the first non-`None` result.
			#[inline] fn find_map<B, F>(&mut self, f: F) -> Result<Option<B>, Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<Option<B>, Self::Error>,
			{
				self.filter_map(f).next()
			}
			/// Returns the position of the first element of this iterator that matches
			/// a predicate. The predicate may fail; such failures are returned to the
			/// caller.
			#[inline] fn position<F>(&mut self, mut f: F) -> Result<Option<usize>, Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<bool, Self::Error>,
			{
				self.try_fold(0, |n, v| {
					if f(v)? {
						return Err(FoldStop::Break(Some(n)));
					}
					Ok(n + 1)
				})
				.map(|_| None)
				.unpack_fold()
			}
			/// Returns the maximal element of the iterator.
			#[inline] fn max(self) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				Self::Item: Ord,
			{
				self.max_by(|a, b| Ok(a.cmp(b)))
			}
			/// Returns the element of the iterator which gives the maximum value from
			/// the function.
			#[inline] fn max_by_key<B, F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				B: Ord,
				F: FnMut(&Self::Item) -> Result<B, Self::Error>,
			{
				let max = match self.next()? {
					Some(v) => (f(&v)?, v),
					None => return Ok(None),
				};

				self.fold(max, |(key, max), v| {
					let new_key = f(&v)?;
					if key > new_key {
						Ok((key, max))
					} else {
						Ok((new_key, v))
					}
				})
				.map(|v| Some(v.1))
			}
			/// Returns the element that gives the maximum value with respect to the function.
			#[inline] fn max_by<F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				F: FnMut(&Self::Item, &Self::Item) -> Result<Ordering, Self::Error>,
			{
				let max = match self.next()? {
					Some(v) => v,
					None => return Ok(None),
				};

				self.fold(max, |max, v| {
					if f(&max, &v)? == Ordering::Greater {
						Ok(max)
					} else {
						Ok(v)
					}
				})
				.map(Some)
			}
			/// Returns the minimal element of the iterator.
			#[inline] fn min(self) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				Self::Item: Ord,
			{
				self.min_by(|a, b| Ok(a.cmp(b)))
			}
			/// Returns the element of the iterator which gives the minimum value from
			/// the function.
			#[inline] fn min_by_key<B, F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				B: Ord,
				F: FnMut(&Self::Item) -> Result<B, Self::Error>,
			{
				let min = match self.next()? {
					Some(v) => (f(&v)?, v),
					None => return Ok(None),
				};

				self.fold(min, |(key, min), v| {
					let new_key = f(&v)?;
					if key < new_key {
						Ok((key, min))
					} else {
						Ok((new_key, v))
					}
				})
				.map(|v| Some(v.1))
			}
			/// Returns the element that gives the minimum value with respect to the function.
			#[inline] fn min_by<F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				F: FnMut(&Self::Item, &Self::Item) -> Result<Ordering, Self::Error>,
			{
				let min = match self.next()? {
					Some(v) => v,
					None => return Ok(None),
				};

				self.fold(min, |min, v| {
					if f(&min, &v)? == Ordering::Less {
						Ok(min)
					} else {
						Ok(v)
					}
				})
				.map(Some)
			}
			/// Returns an iterator that yields this iterator's items in the opposite
			/// order.
			#[inline] fn rev(self) -> Rev<Self>
			where
				Self: Sized + DoubleEndedFallibleIterator,
			{
				Rev(self)
			}
			/// Converts an iterator of pairs into a pair of containers.
			#[inline] fn unzip<A, B, FromA, FromB>(self) -> Result<(FromA, FromB), Self::Error>
			where
				Self: Sized + FallibleIterator<Item = (A, B)>,
				FromA: Default + Extend<A>,
				FromB: Default + Extend<B>,
			{
				let mut from_a = FromA::default();
				let mut from_b = FromB::default();

				self.for_each(|(a, b)| {
					from_a.extend(Some(a));
					from_b.extend(Some(b));
					Ok(())
				})?;

				Ok((from_a, from_b))
			}
			/// Returns an iterator which clones all of its elements.
			#[inline] fn cloned<'a, T>(self) -> Cloned<Self>
			where
				Self: Sized + FallibleIterator<Item = &'a T>,
				T: 'a + Clone,
			{
				Cloned(self)
			}
			/// Returns an iterator which repeats this iterator endlessly.
			#[inline] fn cycle(self) -> Cycle<Self>
			where
				Self: Sized + Clone,
			{
				Cycle {
					it: self.clone(),
					cur: self,
				}
			}
			/// Lexicographically compares the elements of this iterator to that of
			/// another.
			#[inline] fn cmp<I>(mut self, other: I) -> Result<Ordering, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Item = Self::Item, Error = Self::Error>,
				Self::Item: Ord,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(Ordering::Equal),
						(None, _) => return Ok(Ordering::Less),
						(_, None) => return Ok(Ordering::Greater),
						(Some(x), Some(y)) => match x.cmp(&y) {
							Ordering::Equal => {}
							o => return Ok(o),
						},
					}
				}
			}
			/// Lexicographically compares the elements of this iterator to that of
			/// another.
			#[inline] fn partial_cmp<I>(mut self, other: I) -> Result<Option<Ordering>, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialOrd<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(Some(Ordering::Equal)),
						(None, _) => return Ok(Some(Ordering::Less)),
						(_, None) => return Ok(Some(Ordering::Greater)),
						(Some(x), Some(y)) => match x.partial_cmp(&y) {
							Some(Ordering::Equal) => {}
							o => return Ok(o),
						},
					}
				}
			}
			/// Determines if the elements of this iterator are equal to those of
			/// another.
			#[inline] fn eq<I>(mut self, other: I) -> Result<bool, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialEq<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(true),
						(None, _) | (_, None) => return Ok(false),
						(Some(x), Some(y)) => {
							if x != y {
								return Ok(false);
							}
						}
					}
				}
			}
			/// Determines if the elements of this iterator are not equal to those of
			/// another.
			#[inline] fn ne<I>(mut self, other: I) -> Result<bool, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialEq<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(false),
						(None, _) | (_, None) => return Ok(true),
						(Some(x), Some(y)) => {
							if x != y {
								return Ok(true);
							}
						}
					}
				}
			}
			/// Determines if the elements of this iterator are lexicographically less
			/// than those of another.
			#[inline] fn lt<I>(mut self, other: I) -> Result<bool, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialOrd<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(false),
						(None, _) => return Ok(true),
						(_, None) => return Ok(false),
						(Some(x), Some(y)) => match x.partial_cmp(&y) {
							Some(Ordering::Less) => return Ok(true),
							Some(Ordering::Equal) => {}
							Some(Ordering::Greater) => return Ok(false),
							None => return Ok(false),
						},
					}
				}
			}
			/// Determines if the elements of this iterator are lexicographically less
			/// than or equal to those of another.
			#[inline] fn le<I>(mut self, other: I) -> Result<bool, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialOrd<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(true),
						(None, _) => return Ok(true),
						(_, None) => return Ok(false),
						(Some(x), Some(y)) => match x.partial_cmp(&y) {
							Some(Ordering::Less) => return Ok(true),
							Some(Ordering::Equal) => {}
							Some(Ordering::Greater) => return Ok(false),
							None => return Ok(false),
						},
					}
				}
			}
			/// Determines if the elements of this iterator are lexicographically
			/// greater than those of another.
			#[inline] fn gt<I>(mut self, other: I) -> Result<bool, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialOrd<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(false),
						(None, _) => return Ok(false),
						(_, None) => return Ok(true),
						(Some(x), Some(y)) => match x.partial_cmp(&y) {
							Some(Ordering::Less) => return Ok(false),
							Some(Ordering::Equal) => {}
							Some(Ordering::Greater) => return Ok(true),
							None => return Ok(false),
						},
					}
				}
			}
			/// Determines if the elements of this iterator are lexicographically
			/// greater than or equal to those of another.
			#[inline] fn ge<I>(mut self, other: I) -> Result<bool, Self::Error>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
				Self::Item: PartialOrd<I::Item>,
			{
				let mut other = other.into_fallible_iter();

				loop {
					match (self.next()?, other.next()?) {
						(None, None) => return Ok(true),
						(None, _) => return Ok(false),
						(_, None) => return Ok(true),
						(Some(x), Some(y)) => match x.partial_cmp(&y) {
							Some(Ordering::Less) => return Ok(false),
							Some(Ordering::Equal) => {}
							Some(Ordering::Greater) => return Ok(true),
							None => return Ok(false),
						},
					}
				}
			}
			/// Returns a normal (non-fallible) iterator over `Result<Item, Error>`.
			#[inline] fn iterator(self) -> Iterator<Self>
			where
				Self: Sized,
			{
				Iterator(self)
			}
			/// Returns an iterator which applies a transform to the errors of the
			/// underlying iterator.
			#[inline] fn map_err<B, F>(self, f: F) -> MapErr<Self, F>
			where
				F: FnMut(Self::Error) -> B,
				Self: Sized,
			{
				MapErr { it: self, f }
			}
			/// Returns an iterator which unwraps all of its elements.
			#[inline] fn unwrap<T>(self) -> Unwrap<Self>
			where
				Self: Sized + FallibleIterator<Item = T>,
				Self::Error: core::fmt::Debug,
			{
				Unwrap(self)
			}
		}

		impl<I: FallibleIterator + ?Sized> FallibleIterator for &mut I {
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(**self).size_hint()
			}

			#[inline] fn nth(&mut self, n: usize) -> Result<Option<I::Item>, I::Error> {
				(**self).nth(n)
			}
		}

		impl<I: DoubleEndedFallibleIterator + ?Sized> DoubleEndedFallibleIterator for &mut I {
			#[inline] fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next_back()
			}
		}

		#[cfg(feature = "alloc")]
		impl<I: FallibleIterator + ?Sized> FallibleIterator for Box<I> {
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(**self).size_hint()
			}

			#[inline] fn nth(&mut self, n: usize) -> Result<Option<I::Item>, I::Error> {
				(**self).nth(n)
			}
		}

		#[cfg(feature = "alloc")]
		impl<I: DoubleEndedFallibleIterator + ?Sized> DoubleEndedFallibleIterator for Box<I> {
			#[inline] fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next_back()
			}
		}
		/// A fallible iterator able to yield elements from both ends.
		pub trait DoubleEndedFallibleIterator: FallibleIterator {
			/// Advances the end of the iterator, returning the last value.
			fn next_back(&mut self) -> Result<Option<Self::Item>, Self::Error>;

			/// Applies a function over the elements of the iterator in reverse order, producing a single final value.
			#[inline] fn rfold<B, F>(mut self, init: B, f: F) -> Result<B, Self::Error>
			where
				Self: Sized,
				F: FnMut(B, Self::Item) -> Result<B, Self::Error>,
			{
				self.try_rfold(init, f)
			}
			/// Applies a function over the elements of the iterator in reverse, producing a single final value.
			///
			/// This is used as the "base" of many methods on `DoubleEndedFallibleIterator`.
			#[inline] fn try_rfold<B, E, F>(&mut self, mut init: B, mut f: F) -> Result<B, E>
			where
				Self: Sized,
				E: From<Self::Error>,
				F: FnMut(B, Self::Item) -> Result<B, E>,
			{
				while let Some(v) = self.next_back()? {
					init = f(init, v)?;
				}
				Ok(init)
			}
		}
		/// Conversion into a `FallibleIterator`.
		pub trait IntoFallibleIterator {
			/// The elements of the iterator.
			type Item;

			/// The error value of the iterator.
			type Error;

			/// The iterator.
			type IntoFallibleIter: FallibleIterator<Item = Self::Item, Error = Self::Error>;

			/// Creates a fallible iterator from a value.
			fn into_fallible_iter(self) -> Self::IntoFallibleIter;
		}

		impl<I> IntoFallibleIterator for I
		where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;
			type IntoFallibleIter = I;

			#[inline] fn into_fallible_iter(self) -> I {
				self
			}
		}
		/// An iterator which applies a fallible transform to the elements of the underlying iterator.
		#[derive(Clone)]
		pub struct Map<T, F> {
			it: T,
			f: F,
		}

		impl<I: core::fmt::Debug, F> core::fmt::Debug for Map<I, F>
        {
			fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result
            {
				f.debug_struct("Map").field("iter", &self.it).finish()
			}
		}

		impl<T, F, B> FallibleIterator for Map<T, F> where
			T: FallibleIterator,
			F: FnMut(T::Item) -> Result<B, T::Error>,
		{
			type Item = B;
			type Error = T::Error;

			#[inline] fn next(&mut self) -> Result<Option<B>, T::Error> {
				match self.it.next() {
					Ok(Some(v)) => Ok(Some((self.f)(v)?)),
					Ok(None) => Ok(None),
					Err(e) => Err(e),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline] fn try_fold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<T::Error>,
				G: FnMut(C, B) -> Result<C, E>,
			{
				let map = &mut self.f;
				self.it.try_fold(init, |b, v| f(b, map(v)?))
			}
		}

		impl<B, F, I> DoubleEndedFallibleIterator for Map<I, F> where
			I: DoubleEndedFallibleIterator,
			F: FnMut(I::Item) -> Result<B, I::Error>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<B>, I::Error> {
				match self.it.next_back() {
					Ok(Some(v)) => Ok(Some((self.f)(v)?)),
					Ok(None) => Ok(None),
					Err(e) => Err(e),
				}
			}

			#[inline] fn try_rfold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<I::Error>,
				G: FnMut(C, B) -> Result<C, E>,
			{
				let map = &mut self.f;
				self.it.try_rfold(init, |acc, v| f(acc, map(v)?))
			}
		}

		#[derive(Clone, Debug)]
		enum ChainState {
			Both,
			Front,
			Back,
		}
		/// An iterator which yields the elements of one iterator followed by another.
		#[derive(Clone, Debug)]
		pub struct Chain<T, U> {
			front: T,
			back: U,
			state: ChainState,
		}

		impl<T, U> FallibleIterator for Chain<T, U> where
			T: FallibleIterator,
			U: FallibleIterator<Item = T::Item, Error = T::Error>,
		{
			type Item = T::Item;
			type Error = T::Error;

			#[inline] fn next(&mut self) -> Result<Option<T::Item>, T::Error> {
				match self.state {
					ChainState::Both => match self.front.next()? {
						Some(e) => Ok(Some(e)),
						None => {
							self.state = ChainState::Back;
							self.back.next()
						}
					},
					ChainState::Front => self.front.next(),
					ChainState::Back => self.back.next(),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let front_hint = self.front.size_hint();
				let back_hint = self.back.size_hint();

				let low = front_hint.0.saturating_add(back_hint.0);
				let high = match (front_hint.1, back_hint.1) {
					(Some(f), Some(b)) => f.checked_add(b),
					_ => None,
				};

				(low, high)
			}

			#[inline] fn count(self) -> Result<usize, T::Error> {
				match self.state {
					ChainState::Both => Ok(self.front.count()? + self.back.count()?),
					ChainState::Front => self.front.count(),
					ChainState::Back => self.back.count(),
				}
			}

			#[inline] fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<T::Error>,
				F: FnMut(B, T::Item) -> Result<B, E>,
			{
				match self.state {
					ChainState::Both => {
						let init = self.front.try_fold(init, &mut f)?;
						self.state = ChainState::Back;
						self.back.try_fold(init, f)
					}
					ChainState::Front => self.front.try_fold(init, f),
					ChainState::Back => self.back.try_fold(init, f),
				}
			}

			#[inline] fn find<F>(&mut self, mut f: F) -> Result<Option<T::Item>, T::Error>
			where
				F: FnMut(&T::Item) -> Result<bool, T::Error>,
			{
				match self.state {
					ChainState::Both => match self.front.find(&mut f)? {
						Some(v) => Ok(Some(v)),
						None => {
							self.state = ChainState::Back;
							self.back.find(f)
						}
					},
					ChainState::Front => self.front.find(f),
					ChainState::Back => self.back.find(f),
				}
			}

			#[inline] fn last(self) -> Result<Option<T::Item>, T::Error> {
				match self.state {
					ChainState::Both => {
						self.front.last()?;
						self.back.last()
					}
					ChainState::Front => self.front.last(),
					ChainState::Back => self.back.last(),
				}
			}
		}

		impl<T, U> DoubleEndedFallibleIterator for Chain<T, U> where
			T: DoubleEndedFallibleIterator,
			U: DoubleEndedFallibleIterator<Item = T::Item, Error = T::Error>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<T::Item>, T::Error> {
				match self.state {
					ChainState::Both => match self.back.next_back()? {
						Some(e) => Ok(Some(e)),
						None => {
							self.state = ChainState::Front;
							self.front.next_back()
						}
					},
					ChainState::Front => self.front.next_back(),
					ChainState::Back => self.back.next_back(),
				}
			}

			#[inline] fn try_rfold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<T::Error>,
				F: FnMut(B, T::Item) -> Result<B, E>,
			{
				match self.state {
					ChainState::Both => {
						let init = self.back.try_rfold(init, &mut f)?;
						self.state = ChainState::Front;
						self.front.try_rfold(init, f)
					}
					ChainState::Front => self.front.try_rfold(init, f),
					ChainState::Back => self.back.try_rfold(init, f),
				}
			}
		}
		/// An iterator which clones the elements of the underlying iterator.
		#[derive(Clone, Debug)]
		pub struct Cloned<I>(I);

		impl<'a, T, I> FallibleIterator for Cloned<I> where
			I: FallibleIterator<Item = &'a T>,
			T: 'a + Clone,
		{
			type Item = T;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<T>, I::Error> {
				self.0.next().map(|o| o.cloned())
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline] fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, T) -> Result<B, E>,
			{
				self.0.try_fold(init, |acc, v| f(acc, v.clone()))
			}
		}

		impl<'a, T, I> DoubleEndedFallibleIterator for Cloned<I> where
			I: DoubleEndedFallibleIterator<Item = &'a T>,
			T: 'a + Clone,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<T>, I::Error> {
				self.0.next_back().map(|o| o.cloned())
			}

			#[inline] fn try_rfold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, T) -> Result<B, E>,
			{
				self.0.try_rfold(init, |acc, v| f(acc, v.clone()))
			}
		}
		/// Converts an `Iterator<Item = Result<T, E>>` into a `FallibleIterator<Item = T, Error = E>`.
		#[inline] pub fn convert<T, E, I>(it: I) -> Convert<I> where
			I: iter::Iterator<Item = Result<T, E>>,
		{
			Convert(it)
		}
		/// A fallible iterator that wraps a normal iterator over `Result`s.
		#[derive(Clone, Debug)]
		pub struct Convert<I>(I);

		impl<T, E, I> FallibleIterator for Convert<I> where
			I: iter::Iterator<Item = Result<T, E>>,
		{
			type Item = T;
			type Error = E;

			#[inline] fn next(&mut self) -> Result<Option<T>, E> {
				match self.0.next() {
					Some(Ok(i)) => Ok(Some(i)),
					Some(Err(e)) => Err(e),
					None => Ok(None),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline] fn try_fold<B, E2, F>(&mut self, init: B, mut f: F) -> Result<B, E2>
			where
				E2: From<E>,
				F: FnMut(B, T) -> Result<B, E2>,
			{
				self.0.try_fold(init, |acc, v| f(acc, v?))
			}
		}

		impl<T, E, I> DoubleEndedFallibleIterator for Convert<I> where
			I: DoubleEndedIterator<Item = Result<T, E>>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<T>, E> {
				match self.0.next_back() {
					Some(Ok(i)) => Ok(Some(i)),
					Some(Err(e)) => Err(e),
					None => Ok(None),
				}
			}

			#[inline] fn try_rfold<B, E2, F>(&mut self, init: B, mut f: F) -> Result<B, E2>
			where
				E2: From<E>,
				F: FnMut(B, T) -> Result<B, E2>,
			{
				self.0.try_rfold(init, |acc, v| f(acc, v?))
			}
		}
		/// A fallible iterator that wraps a normal iterator over `Result`s.
		#[derive(Clone, Debug)]
		pub struct IntoFallible<I>(I);

		impl<T, I> FallibleIterator for IntoFallible<I> where
			I: iter::Iterator<Item = T>,
		{
			type Item = T;
			type Error = Infallible;

			#[inline] fn next(&mut self) -> Result<Option<T>, Self::Error> {
				Ok(self.0.next())
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline] fn try_fold<B, E2, F>(&mut self, init: B, f: F) -> Result<B, E2>
			where
				E2: From<Infallible>,
				F: FnMut(B, T) -> Result<B, E2>,
			{
				self.0.try_fold(init, f)
			}
		}

		impl<T, I: iter::Iterator<Item = T>> From<I> for IntoFallible<I>
        {
			fn from(value: I) -> Self {
				Self(value)
			}
		}

		impl<T, I> DoubleEndedFallibleIterator for IntoFallible<I> where
			I: DoubleEndedIterator<Item = T>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<T>, Infallible> {
				Ok(self.0.next_back())
			}

			#[inline] fn try_rfold<B, E2, F>(&mut self, init: B, f: F) -> Result<B, E2>
			where
				E2: From<Infallible>,
				F: FnMut(B, T) -> Result<B, E2>,
			{
				self.0.try_rfold(init, f)
			}
		}
		/// An iterator that yields the iteration count as well as the values of the
		/// underlying iterator.
		#[derive(Clone, Debug)]
		pub struct Enumerate<I> {
			it: I,
			n: usize,
		}

		impl<I> FallibleIterator for Enumerate<I> where
			I: FallibleIterator,
		{
			type Item = (usize, I::Item);
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<(usize, I::Item)>, I::Error> {
				self.it.next().map(|o| {
					o.map(|e| {
						let i = self.n;
						self.n += 1;
						(i, e)
					})
				})
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline] fn count(self) -> Result<usize, I::Error> {
				self.it.count()
			}

			#[inline] fn nth(&mut self, n: usize) -> Result<Option<(usize, I::Item)>, I::Error> {
				match self.it.nth(n)? {
					Some(v) => {
						let i = self.n + n;
						self.n = i + 1;
						Ok(Some((i, v)))
					}
					None => Ok(None),
				}
			}

			#[inline] fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, (usize, I::Item)) -> Result<B, E>,
			{
				let n = &mut self.n;
				self.it.try_fold(init, |acc, v| {
					let i = *n;
					*n += 1;
					f(acc, (i, v))
				})
			}
		}
		/// An iterator which uses a fallible predicate to determine which values of the
		/// underlying iterator should be yielded.
		#[derive(Clone, Debug)]
		pub struct Filter<I, F> {
			it: I,
			f: F,
		}

		impl<I, F> FallibleIterator for Filter<I, F> where
			I: FallibleIterator,
			F: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error>
            {
				let filter = &mut self.f;
				self.it
					.try_fold((), |(), v| {
						if filter(&v)? {
							return Err(FoldStop::Break(Some(v)));
						}
						Ok(())
					})
					.map(|()| None)
					.unpack_fold()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.it.size_hint().1)
			}

			#[inline] fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
			where
				E: From<I::Error>,
				G: FnMut(B, I::Item) -> Result<B, E>,
			{
				let predicate = &mut self.f;
				self.it.try_fold(
					init,
					|acc, v| {
						if predicate(&v)? {
							f(acc, v)
						} else {
							Ok(acc)
						}
					},
				)
			}
		}

		impl<I, F> DoubleEndedFallibleIterator for Filter<I, F> where
			I: DoubleEndedFallibleIterator,
			F: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<I::Item>, I::Error>
            {
				let filter = &mut self.f;
				self.it
					.try_rfold((), |(), v| {
						if filter(&v)? {
							return Err(FoldStop::Break(Some(v)));
						}
						Ok(())
					})
					.map(|()| None)
					.unpack_fold()
			}

			#[inline] fn try_rfold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
			where
				E: From<I::Error>,
				G: FnMut(B, I::Item) -> Result<B, E>,
			{
				let predicate = &mut self.f;
				self.it.try_rfold(
					init,
					|acc, v| {
						if predicate(&v)? {
							f(acc, v)
						} else {
							Ok(acc)
						}
					},
				)
			}
		}
		/// An iterator which both filters and maps the values of the underlying
		/// iterator.
		#[derive(Clone, Debug)]
		pub struct FilterMap<I, F> {
			it: I,
			f: F,
		}

		impl<B, I, F> FallibleIterator for FilterMap<I, F> where
			I: FallibleIterator,
			F: FnMut(I::Item) -> Result<Option<B>, I::Error>,
		{
			type Item = B;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<B>, I::Error>
            {
				let map = &mut self.f;
				self.it
					.try_fold((), |(), v| match map(v)? {
						Some(v) => Err(FoldStop::Break(Some(v))),
						None => Ok(()),
					})
					.map(|()| None)
					.unpack_fold()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.it.size_hint().1)
			}

			#[inline] fn try_fold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<I::Error>,
				G: FnMut(C, B) -> Result<C, E>,
			{
				let map = &mut self.f;
				self.it.try_fold(init, |acc, v| match map(v)? {
					Some(v) => f(acc, v),
					None => Ok(acc),
				})
			}
		}

		impl<B, I, F> DoubleEndedFallibleIterator for FilterMap<I, F> where
			I: DoubleEndedFallibleIterator,
			F: FnMut(I::Item) -> Result<Option<B>, I::Error>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<B>, I::Error>
            {
				let map = &mut self.f;
				self.it
					.try_rfold((), |(), v| match map(v)? {
						Some(v) => Err(FoldStop::Break(Some(v))),
						None => Ok(()),
					})
					.map(|()| None)
					.unpack_fold()
			}

			#[inline] fn try_rfold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<I::Error>,
				G: FnMut(C, B) -> Result<C, E>,
			{
				let map = &mut self.f;
				self.it.try_rfold(init, |acc, v| match map(v)? {
					Some(v) => f(acc, v),
					None => Ok(acc),
				})
			}
		}
		/// An iterator which maps each element to another iterator, yielding those iterator's elements.
		#[derive(Clone, Debug)]
		pub struct FlatMap<I, U, F> where
			U: IntoFallibleIterator,
		{
			it: Map<I, F>,
			cur: Option<U::IntoFallibleIter>,
		}

		impl<I, U, F> FallibleIterator for FlatMap<I, U, F> where
			I: FallibleIterator,
			U: IntoFallibleIterator<Error = I::Error>,
			F: FnMut(I::Item) -> Result<U, I::Error>,
		{
			type Item = U::Item;
			type Error = U::Error;

			#[inline] fn next(&mut self) -> Result<Option<U::Item>, U::Error> {
				loop {
					if let Some(it) = &mut self.cur {
						if let Some(v) = it.next()? {
							return Ok(Some(v));
						}
					}
					match self.it.next()? {
						Some(it) => self.cur = Some(it.into_fallible_iter()),
						None => return Ok(None),
					}
				}
			}

			#[inline] fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
			where
				E: From<U::Error>,
				G: FnMut(B, U::Item) -> Result<B, E>,
			{
				let mut acc = init;
				if let Some(cur) = &mut self.cur {
					acc = cur.try_fold(acc, &mut f)?;
					self.cur = None;
				}

				let cur = &mut self.cur;
				self.it.try_fold(acc, |acc, v| {
					let mut it = v.into_fallible_iter();
					match it.try_fold(acc, &mut f) {
						Ok(acc) => Ok(acc),
						Err(e) => {
							*cur = Some(it);
							Err(e)
						}
					}
				})
			}
		}
		/// An iterator which flattens an iterator of iterators, yielding those iterators' elements.
		pub struct Flatten<I> where
			I: FallibleIterator,
			I::Item: IntoFallibleIterator,
		{
			it: I,
			cur: Option<<I::Item as IntoFallibleIterator>::IntoFallibleIter>,
		}

		impl<I> Clone for Flatten<I> where
			I: FallibleIterator + Clone,
			I::Item: IntoFallibleIterator,
			<I::Item as IntoFallibleIterator>::IntoFallibleIter: Clone,
		{
			#[inline] fn clone(&self) -> Flatten<I> {
				Flatten {
					it: self.it.clone(),
					cur: self.cur.clone(),
				}
			}
		}

		impl<I> FallibleIterator for Flatten<I> where
			I: FallibleIterator,
			I::Item: IntoFallibleIterator<Error = I::Error>,
		{
			type Item = <I::Item as IntoFallibleIterator>::Item;
			type Error = <I::Item as IntoFallibleIterator>::Error;

			#[inline] fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				loop {
					if let Some(it) = &mut self.cur {
						if let Some(v) = it.next()? {
							return Ok(Some(v));
						}
					}
					match self.it.next()? {
						Some(it) => self.cur = Some(it.into_fallible_iter()),
						None => return Ok(None),
					}
				}
			}

			#[inline] fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
			where
				E: From<Self::Error>,
				G: FnMut(B, Self::Item) -> Result<B, E>,
			{
				let mut acc = init;
				if let Some(cur) = &mut self.cur {
					acc = cur.try_fold(acc, &mut f)?;
					self.cur = None;
				}

				let cur = &mut self.cur;
				self.it.try_fold(acc, |acc, v| {
					let mut it = v.into_fallible_iter();
					match it.try_fold(acc, &mut f) {
						Ok(acc) => Ok(acc),
						Err(e) => {
							*cur = Some(it);
							Err(e)
						}
					}
				})
			}
		}
		/// Creates an iterator from a fallible function generating values.
		#[inline] pub fn from_fn<I, E, F>(fun: F) -> FromFn<F> where
			F: FnMut() -> Result<Option<I>, E>,
		{
			FromFn { fun }
		}
		/// An iterator using a function to generate new values.
		#[derive(Clone, Debug)]
		pub struct FromFn<F> {
			fun: F,
		}

		impl<I, E, F> FallibleIterator for FromFn<F> where
			F: FnMut() -> Result<Option<I>, E>,
		{
			type Item = I;
			type Error = E;

			fn next(&mut self) -> Result<Option<I>, E> {
				(self.fun)()
			}
		}
		/// An iterator that yields `Ok(None)` forever after the underlying iterator yields `Ok(None)` once.
		#[derive(Clone, Debug)]
		pub struct Fuse<I> {
			it: I,
			done: bool,
		}

		impl<I> FallibleIterator for Fuse<I> where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.done {
					return Ok(None);
				}

				match self.it.next()? {
					Some(i) => Ok(Some(i)),
					None => {
						self.done = true;
						Ok(None)
					}
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				if self.done {
					(0, Some(0))
				} else {
					self.it.size_hint()
				}
			}

			#[inline] fn count(self) -> Result<usize, I::Error> {
				if self.done {
					Ok(0)
				} else {
					self.it.count()
				}
			}

			#[inline] fn last(self) -> Result<Option<I::Item>, I::Error> {
				if self.done {
					Ok(None)
				} else {
					self.it.last()
				}
			}

			#[inline] fn nth(&mut self, n: usize) -> Result<Option<I::Item>, I::Error> {
				if self.done {
					Ok(None)
				} else {
					let v = self.it.nth(n)?;
					if v.is_none() {
						self.done = true;
					}
					Ok(v)
				}
			}

			#[inline] fn try_fold<B, E, F>(&mut self, init: B, f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, I::Item) -> Result<B, E>,
			{
				if self.done {
					Ok(init)
				} else {
					self.it.try_fold(init, f)
				}
			}
		}
		/// An iterator which passes each element to a closure before returning it.
		#[derive(Clone, Debug)]
		pub struct Inspect<I, F> {
			it: I,
			f: F,
		}

		impl<I, F> FallibleIterator for Inspect<I, F> where
			I: FallibleIterator,
			F: FnMut(&I::Item) -> Result<(), I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				match self.it.next()? {
					Some(i) => {
						(self.f)(&i)?;
						Ok(Some(i))
					}
					None => Ok(None),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline] fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
			where
				E: From<I::Error>,
				G: FnMut(B, I::Item) -> Result<B, E>,
			{
				let inspect = &mut self.f;
				self.it.try_fold(init, |acc, v| {
					inspect(&v)?;
					f(acc, v)
				})
			}
		}

		impl<I, F> DoubleEndedFallibleIterator for Inspect<I, F> where
			I: DoubleEndedFallibleIterator,
			F: FnMut(&I::Item) -> Result<(), I::Error>,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				match self.it.next_back()? {
					Some(i) => {
						(self.f)(&i)?;
						Ok(Some(i))
					}
					None => Ok(None),
				}
			}

			#[inline] fn try_rfold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
			where
				E: From<I::Error>,
				G: FnMut(B, I::Item) -> Result<B, E>,
			{
				let inspect = &mut self.f;
				self.it.try_rfold(init, |acc, v| {
					inspect(&v)?;
					f(acc, v)
				})
			}
		}
		/// A normal (non-fallible) iterator which wraps a fallible iterator.
		#[derive(Clone, Debug)]
		pub struct Iterator<I>(I);

		impl<I> iter::Iterator for Iterator<I> where
			I: FallibleIterator,
		{
			type Item = Result<I::Item, I::Error>;

			#[inline] fn next(&mut self) -> Option<Result<I::Item, I::Error>> {
				match self.0.next() {
					Ok(Some(v)) => Some(Ok(v)),
					Ok(None) => None,
					Err(e) => Some(Err(e)),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}
		}

		impl<I> DoubleEndedIterator for Iterator<I> where
			I: DoubleEndedFallibleIterator,
		{
			#[inline] fn next_back(&mut self) -> Option<Result<I::Item, I::Error>> {
				match self.0.next_back() {
					Ok(Some(v)) => Some(Ok(v)),
					Ok(None) => None,
					Err(e) => Some(Err(e)),
				}
			}
		}
		/// An iterator which applies a transform to the errors of the underlying iterator.
		#[derive(Clone, Debug)]
		pub struct MapErr<I, F> {
			it: I,
			f: F,
		}

		impl<B, F, I> FallibleIterator for MapErr<I, F> where
			I: FallibleIterator,
			F: FnMut(I::Error) -> B,
		{
			type Item = I::Item;
			type Error = B;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, B> {
				self.it.next().map_err(&mut self.f)
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline] fn count(mut self) -> Result<usize, B> {
				self.it.count().map_err(&mut self.f)
			}

			#[inline] fn last(mut self) -> Result<Option<I::Item>, B> {
				self.it.last().map_err(&mut self.f)
			}

			#[inline] fn nth(&mut self, n: usize) -> Result<Option<I::Item>, B> {
				self.it.nth(n).map_err(&mut self.f)
			}

			#[inline] fn try_fold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<B>,
				G: FnMut(C, I::Item) -> Result<C, E>,
			{
				self.it
					.try_fold(init, |acc, v| f(acc, v).map_err(MappedErr::Fold))
					.map_err(|e| match e {
						MappedErr::It(e) => (self.f)(e).into(),
						MappedErr::Fold(e) => e,
					})
			}
		}

		impl<B, F, I> DoubleEndedFallibleIterator for MapErr<I, F> where
			I: DoubleEndedFallibleIterator,
			F: FnMut(I::Error) -> B,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<I::Item>, B> {
				self.it.next_back().map_err(&mut self.f)
			}

			#[inline] fn try_rfold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<B>,
				G: FnMut(C, I::Item) -> Result<C, E>,
			{
				self.it
					.try_rfold(init, |acc, v| f(acc, v).map_err(MappedErr::Fold))
					.map_err(|e| match e {
						MappedErr::It(e) => (self.f)(e).into(),
						MappedErr::Fold(e) => e,
					})
			}
		}

		enum MappedErr<T, U> {
			It(T),
			Fold(U),
		}

		impl<T, U> From<T> for MappedErr<T, U> {
			#[inline] fn from(t: T) -> MappedErr<T, U> {
				MappedErr::It(t)
			}
		}
		/// An iterator which can look at the next element without consuming it.
		#[derive(Clone, Debug)]
		pub struct Peekable<I: FallibleIterator>
        {
			it: I,
			next: Option<I::Item>,
		}

		impl<I> Peekable<I> where
			I: FallibleIterator,
		{
			/// Returns a reference to the next value without advancing the iterator.
			#[inline] pub fn peek(&mut self) -> Result<Option<&I::Item>, I::Error> {
				if self.next.is_none() {
					self.next = self.it.next()?;
				}

				Ok(self.next.as_ref())
			}
			/// Consume and return the next value of this iterator if a condition is true.
			///
			/// If func returns true for the next value of this iterator, consume and return it. Otherwise, return None.
			#[inline] pub fn next_if(&mut self, f: impl Fn(&I::Item) -> bool) -> Result<Option<I::Item>, I::Error> {
				match self.peek()? {
					Some(item) if f(item) => self.next(),
					_ => Ok(None),
				}
			}
			/// Consume and return the next item if it is equal to `expected`.
			#[inline] pub fn next_if_eq<T>(&mut self, expected: &T) -> Result<Option<I::Item>, I::Error>
			where
				T: ?Sized,
				I::Item: PartialEq<T>,
			{
				self.next_if(|found| found == expected)
			}
		}

		impl<I> FallibleIterator for Peekable<I> where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if let Some(next) = self.next.take() {
					return Ok(Some(next));
				}

				self.it.next()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let mut hint = self.it.size_hint();
				if self.next.is_some() {
					hint.0 = hint.0.saturating_add(1);
					hint.1 = hint.1.and_then(|h| h.checked_add(1));
				}
				hint
			}

			#[inline] fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, I::Item) -> Result<B, E>,
			{
				let mut acc = init;
				if let Some(v) = self.next.take() {
					acc = f(acc, v)?;
				}
				self.it.try_fold(acc, f)
			}
		}
		/// An iterator which yields elements of the underlying iterator in reverse
		/// order.
		#[derive(Clone, Debug)]
		pub struct Rev<I>(I);

		impl<I> FallibleIterator for Rev<I> where
			I: DoubleEndedFallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				self.0.next_back()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline] fn count(self) -> Result<usize, I::Error> {
				self.0.count()
			}

			#[inline] fn try_fold<B, E, F>(&mut self, init: B, f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, I::Item) -> Result<B, E>,
			{
				self.0.try_rfold(init, f)
			}
		}

		impl<I> DoubleEndedFallibleIterator for Rev<I> where
			I: DoubleEndedFallibleIterator,
		{
			#[inline] fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				self.0.next()
			}

			#[inline] fn try_rfold<B, E, F>(&mut self, init: B, f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, I::Item) -> Result<B, E>,
			{
				self.0.try_fold(init, f)
			}
		}
		/// An iterator which applies a stateful closure.
		#[derive(Clone, Debug)]
		pub struct Scan<I, St, F> {
			it: I,
			f: F,
			state: St,
		}

		impl<B, I, St, F> FallibleIterator for Scan<I, St, F> where
			I: FallibleIterator,
			F: FnMut(&mut St, I::Item) -> Result<Option<B>, I::Error>,
		{
			type Item = B;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<B>, I::Error> {
				match self.it.next()? {
					Some(v) => (self.f)(&mut self.state, v),
					None => Ok(None),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let hint = self.it.size_hint();
				(0, hint.1)
			}
		}
		/// An iterator which skips initial elements.
		#[derive(Clone, Debug)]
		pub struct Skip<I> {
			it: I,
			n: usize,
		}

		impl<I> FallibleIterator for Skip<I> where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.n == 0 {
					self.it.next()
				} else {
					let n = self.n;
					self.n = 0;
					self.it.nth(n)
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let hint = self.it.size_hint();

				(
					hint.0.saturating_sub(self.n),
					hint.1.map(|x| x.saturating_sub(self.n)),
				)
			}
		}
		/// An iterator which skips initial elements based on a predicate.
		#[derive(Clone, Debug)]
		pub struct SkipWhile<I, P> {
			it: I,
			flag: bool,
			predicate: P,
		}

		impl<I, P> FallibleIterator for SkipWhile<I, P> where
			I: FallibleIterator,
			P: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error>
            {
				let flag = &mut self.flag;
				let pred = &mut self.predicate;
				self.it.find(move |x| {
					if *flag || !pred(x)? {
						*flag = true;
						Ok(true)
					} else {
						Ok(false)
					}
				})
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let hint = self.it.size_hint();
				if self.flag {
					hint
				} else {
					(0, hint.1)
				}
			}
		}
		/// An iterator which steps through the elements of the underlying iterator by a certain amount.
		#[derive(Clone, Debug)]
		pub struct StepBy<I> {
			it: I,
			step: usize,
			first_take: bool,
		}

		impl<I> FallibleIterator for StepBy<I> where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.first_take {
					self.first_take = false;
					self.it.next()
				} else {
					self.it.nth(self.step)
				}
			}

			fn size_hint(&self) -> (usize, Option<usize>) {
				let inner_hint = self.it.size_hint();

				if self.first_take {
					let f = |n| {
						if n == 0 {
							0
						} else {
							1 + (n - 1) / (self.step + 1)
						}
					};
					(f(inner_hint.0), inner_hint.1.map(f))
				} else {
					let f = |n| n / (self.step + 1);
					(f(inner_hint.0), inner_hint.1.map(f))
				}
			}
		}
		/// An iterator which yields a limited number of elements from the underlying
		/// iterator.
		#[derive(Clone, Debug)]
		pub struct Take<I> {
			it: I,
			remaining: usize,
		}

		impl<I> FallibleIterator for Take<I> where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.remaining == 0 {
					return Ok(None);
				}

				let next = self.it.next();
				if let Ok(Some(_)) = next {
					self.remaining -= 1;
				}
				next
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let hint = self.it.size_hint();
				(
					cmp::min(hint.0, self.remaining),
					hint.1.map(|n| cmp::min(n, self.remaining)),
				)
			}
		}
		/// An iterator which yields elements based on a predicate.
		#[derive(Clone, Debug)]
		pub struct TakeWhile<I, P> {
			it: I,
			flag: bool,
			predicate: P,
		}

		impl<I, P> FallibleIterator for TakeWhile<I, P> where
			I: FallibleIterator,
			P: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.flag {
					Ok(None)
				} else {
					match self.it.next()? {
						Some(item) => {
							if (self.predicate)(&item)? {
								Ok(Some(item))
							} else {
								self.flag = true;
								Ok(None)
							}
						}
						None => Ok(None),
					}
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				if self.flag {
					(0, Some(0))
				} else {
					let hint = self.it.size_hint();
					(0, hint.1)
				}
			}
		}
		/// An iterator which cycles another endlessly.
		#[derive(Clone, Debug)]
		pub struct Cycle<I> {
			it: I,
			cur: I,
		}

		impl<I> FallibleIterator for Cycle<I> where
			I: FallibleIterator + Clone,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline] fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				match self.cur.next()? {
					None => {
						self.cur = self.it.clone();
						self.cur.next()
					}
					Some(v) => Ok(Some(v)),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(usize::max_value(), None)
			}
		}
		/// An iterator that yields pairs of this iterator's and another iterator's
		/// values.
		#[derive(Clone, Debug)]
		pub struct Zip<T, U>(T, U);

		impl<T, U> FallibleIterator for Zip<T, U> where
			T: FallibleIterator,
			U: FallibleIterator<Error = T::Error>,
		{
			type Item = (T::Item, U::Item);
			type Error = T::Error;

			#[inline] fn next(&mut self) -> Result<Option<(T::Item, U::Item)>, T::Error> {
				match (self.0.next()?, self.1.next()?) {
					(Some(a), Some(b)) => Ok(Some((a, b))),
					_ => Ok(None),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let a = self.0.size_hint();
				let b = self.1.size_hint();

				let low = cmp::min(a.0, b.0);

				let high = match (a.1, b.1) {
					(Some(a), Some(b)) => Some(cmp::min(a, b)),
					(Some(a), None) => Some(a),
					(None, Some(b)) => Some(b),
					(None, None) => None,
				};

				(low, high)
			}
		}
		/// An iterator that unwraps every element yielded by the underlying
		/// FallibleIterator
		#[derive(Clone, Debug)]
		pub struct Unwrap<T>(T);

		impl<T> iter::Iterator for Unwrap<T> where
			T: FallibleIterator,
			T::Error: core::fmt::Debug,
		{
			type Item = T::Item;

			#[inline] fn next(&mut self) -> Option<T::Item> {
				self.0.next().unwrap()
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				let (_, max) = self.0.size_hint();
				(0, max)
			}
		}

		impl<T> iter::DoubleEndedIterator for Unwrap<T> where
			T: DoubleEndedFallibleIterator,
			T::Error: core::fmt::Debug,
		{
			#[inline] fn next_back(&mut self) -> Option<T::Item> {
				self.0.next_back().unwrap()
			}
		}

		fn _is_object_safe(_: &dyn DoubleEndedFallibleIterator<Item = (), Error = ()>) {}
		/// An extnsion-trait with set of useful methods to convert [`core::iter::Iterator`]
		/// into [`FallibleIterator`]
		pub trait IteratorExt {
			/// Convert an iterator of `Result`s into `FallibleIterator` by transposition
			fn transpose_into_fallible<T, E>(self) -> Convert<Self>
			where
				Self: iter::Iterator<Item = Result<T, E>> + Sized;

			/// Convert an iterator of anything into `FallibleIterator` by wrapping
			/// into `Result<T, Infallible>` where `Infallible` is an error that can never actually
			/// happen.
			fn into_fallible<T>(self) -> IntoFallible<Self>
			where
				Self: iter::Iterator<Item = T> + Sized;
		}

		impl<I> IteratorExt for I
		where
			I: iter::Iterator,
		{
			/// Convert an iterator of `Result`s into `FallibleIterator` by transposition
			fn transpose_into_fallible<T, E>(self) -> Convert<Self>
			where
				Self: iter::Iterator<Item = Result<T, E>> + Sized,
			{
				Convert(self)
			}
			/// Convert an iterator of anything into `FallibleIterator` by wrapping
			/// into `Result<T, Infallible>` where `Infallible` is an error that can never actually
			/// happen.
			fn into_fallible<T>(self) -> IntoFallible<Self>
			where
				Self: iter::Iterator<Item = T> + Sized,
			{
				IntoFallible(self)
			}
		}
		/// An iterator that yields nothing.
		#[derive(Clone, Debug)]
		pub struct Empty<T, E>(PhantomData<T>, PhantomData<E>);

		impl<T, E> FallibleIterator for Empty<T, E> {
			type Item = T;
			type Error = E;

			#[inline] fn next(&mut self) -> Result<Option<T>, E> {
				Ok(None)
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(0, Some(0))
			}
		}
		/// Creates an iterator that yields nothing.
		pub fn empty<T, E>() -> Empty<T, E> {
			Empty(PhantomData, PhantomData)
		}
		/// An iterator that yields something exactly once.
		#[derive(Clone, Debug)]
		pub struct Once<T, E>(Option<T>, PhantomData<E>);
		/// Creates an iterator that yields an element exactly once.
		pub fn once<T, E>(value: T) -> Once<T, E> {
			Once(Some(value), PhantomData)
		}

		impl<T, E> FallibleIterator for Once<T, E> {
			type Item = T;
			type Error = E;

			#[inline] fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				Ok(self.0.take())
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				match self.0 {
					Some(_) => (1, Some(1)),
					None => (0, Some(0)),
				}
			}
		}
		/// An iterator that fails with a predetermined error exactly once.
		#[derive(Clone, Debug)]
		pub struct OnceErr<T, E>(PhantomData<T>, Option<E>);
		/// Creates an iterator that fails with a predetermined error exactly once.
		pub fn once_err<T, E>(value: E) -> OnceErr<T, E> {
			OnceErr(PhantomData, Some(value))
		}

		impl<T, E> FallibleIterator for OnceErr<T, E> {
			type Item = T;
			type Error = E;

			#[inline] fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				match self.1.take() {
					Some(value) => Err(value),
					None => Ok(None),
				}
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(0, Some(0))
			}
		}
		/// An iterator that endlessly repeats a single element.
		#[derive(Clone, Debug)]
		pub struct Repeat<T: Clone, E>(T, PhantomData<E>);
		/// Creates an iterator that endlessly repeats a single element.
		pub fn repeat<T: Clone, E>(value: T) -> Repeat<T, E> {
			Repeat(value, PhantomData)
		}

		impl<T: Clone, E> FallibleIterator for Repeat<T, E> {
			type Item = T;
			type Error = E;

			#[inline] fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				Ok(Some(self.0.clone()))
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(usize::max_value(), None)
			}
		}
		/// An iterator that endlessly repeats a single error.
		#[derive(Clone, Debug)]
		pub struct RepeatErr<T, E: Clone>(PhantomData<T>, E);
		/// Creates an iterator that endlessly repeats a single error.
		pub fn repeat_err<T, E: Clone>(value: E) -> RepeatErr<T, E> {
			RepeatErr(PhantomData, value)
		}

		impl<T, E: Clone> FallibleIterator for RepeatErr<T, E>
        {
			type Item = T;
			type Error = E;

			#[inline] fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				Err(self.1.clone())
			}

			#[inline] fn size_hint(&self) -> (usize, Option<usize>) {
				(0, Some(0))
			}
		}

	}
}

pub mod ffi
{
	pub use std::ffi::{ * };
}

pub mod fmt
{
	pub use std::fmt::{ * };
}

pub mod mem
{
	pub use std::mem::{ * };
}

pub mod option
{
	pub use std::option::{ * };
}

pub mod os
{
	pub use std::os::{ * };
}

pub mod panic
{
	pub use std::panic::{ * };
}

pub mod path
{
	pub use std::path::{ * };
}

pub mod result
{
	pub use std::result::{ * };
}

pub mod sqlite3
{
	/*!
	*/
	use ::
	{
		*,
	};
	/*
	*/	
	#[must_use] pub fn SQLITE_STATIC() -> sqlite3_destructor_type { None }

	#[must_use] pub fn SQLITE_TRANSIENT() -> sqlite3_destructor_type
	{ Some(unsafe { mem::transmute::<isize, unsafe extern "C" fn(*mut ::ffi::c_void)>(-1_isize) }) }

	#[allow(dead_code, clippy::all)]
	mod bindings
	{
		/*!
		Automatically generated by rust-bindgen 0.71.1 */
		use ::
		{
			*,
		};
		/*
		*/
		pub type sqlite_int64 = ::os::raw::c_longlong;
		pub type sqlite_uint64 = ::os::raw::c_ulonglong;
		pub type sqlite3_int64 = sqlite_int64;
		pub type sqlite3_uint64 = sqlite_uint64;
		pub type sqlite3_callback = ::option::Option<
			unsafe extern "C" fn(
				arg1: *mut ::os::raw::c_void,
				arg2: ::os::raw::c_int,
				arg3: *mut *mut ::os::raw::c_char,
				arg4: *mut *mut ::os::raw::c_char,
			) -> ::os::raw::c_int,
		>;
		pub type sqlite3_syscall_ptr = ::option::Option<unsafe extern "C" fn()>;		
		pub type sqlite3_value = Mem;
		pub type sqlite3_destructor_type = ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>;
		pub type sqlite3_rtree_dbl = f64;		
		pub type fts5_extension_function = ::option::Option<
			unsafe extern "C" fn(
				pApi: *const Fts5ExtensionApi,
				pFts: *mut Fts5Context,
				pCtx: *mut sqlite3_context,
				nVal: ::os::raw::c_int,
				apVal: *mut *mut sqlite3_value,
			),
		>;
		
		//pub const SQLITE_VERSION: &::ffi::CStr = c"3.14.0";
		pub const SQLITE_VERSION_NUMBER: i32 = 3014000;
		//pub const SQLITE_SOURCE_ID: &::ffi::CStr = c"2016-08-08 13:40:27 d5e98057028abcf7217d0d2b2e29bbbcdf09d6de";
		pub const SQLITE_OK: i32 = 0;
		pub const SQLITE_ERROR: i32 = 1;
		pub const SQLITE_INTERNAL: i32 = 2;
		pub const SQLITE_PERM: i32 = 3;
		pub const SQLITE_ABORT: i32 = 4;
		pub const SQLITE_BUSY: i32 = 5;
		pub const SQLITE_LOCKED: i32 = 6;
		pub const SQLITE_NOMEM: i32 = 7;
		pub const SQLITE_READONLY: i32 = 8;
		pub const SQLITE_INTERRUPT: i32 = 9;
		pub const SQLITE_IOERR: i32 = 10;
		pub const SQLITE_CORRUPT: i32 = 11;
		pub const SQLITE_NOTFOUND: i32 = 12;
		pub const SQLITE_FULL: i32 = 13;
		pub const SQLITE_CANTOPEN: i32 = 14;
		pub const SQLITE_PROTOCOL: i32 = 15;
		pub const SQLITE_EMPTY: i32 = 16;
		pub const SQLITE_SCHEMA: i32 = 17;
		pub const SQLITE_TOOBIG: i32 = 18;
		pub const SQLITE_CONSTRAINT: i32 = 19;
		pub const SQLITE_MISMATCH: i32 = 20;
		pub const SQLITE_MISUSE: i32 = 21;
		pub const SQLITE_NOLFS: i32 = 22;
		pub const SQLITE_AUTH: i32 = 23;
		pub const SQLITE_FORMAT: i32 = 24;
		pub const SQLITE_RANGE: i32 = 25;
		pub const SQLITE_NOTADB: i32 = 26;
		pub const SQLITE_NOTICE: i32 = 27;
		pub const SQLITE_WARNING: i32 = 28;
		pub const SQLITE_ROW: i32 = 100;
		pub const SQLITE_DONE: i32 = 101;
		pub const SQLITE_IOERR_READ: i32 = 266;
		pub const SQLITE_IOERR_SHORT_READ: i32 = 522;
		pub const SQLITE_IOERR_WRITE: i32 = 778;
		pub const SQLITE_IOERR_FSYNC: i32 = 1034;
		pub const SQLITE_IOERR_DIR_FSYNC: i32 = 1290;
		pub const SQLITE_IOERR_TRUNCATE: i32 = 1546;
		pub const SQLITE_IOERR_FSTAT: i32 = 1802;
		pub const SQLITE_IOERR_UNLOCK: i32 = 2058;
		pub const SQLITE_IOERR_RDLOCK: i32 = 2314;
		pub const SQLITE_IOERR_DELETE: i32 = 2570;
		pub const SQLITE_IOERR_BLOCKED: i32 = 2826;
		pub const SQLITE_IOERR_NOMEM: i32 = 3082;
		pub const SQLITE_IOERR_ACCESS: i32 = 3338;
		pub const SQLITE_IOERR_CHECKRESERVEDLOCK: i32 = 3594;
		pub const SQLITE_IOERR_LOCK: i32 = 3850;
		pub const SQLITE_IOERR_CLOSE: i32 = 4106;
		pub const SQLITE_IOERR_DIR_CLOSE: i32 = 4362;
		pub const SQLITE_IOERR_SHMOPEN: i32 = 4618;
		pub const SQLITE_IOERR_SHMSIZE: i32 = 4874;
		pub const SQLITE_IOERR_SHMLOCK: i32 = 5130;
		pub const SQLITE_IOERR_SHMMAP: i32 = 5386;
		pub const SQLITE_IOERR_SEEK: i32 = 5642;
		pub const SQLITE_IOERR_DELETE_NOENT: i32 = 5898;
		pub const SQLITE_IOERR_MMAP: i32 = 6154;
		pub const SQLITE_IOERR_GETTEMPPATH: i32 = 6410;
		pub const SQLITE_IOERR_CONVPATH: i32 = 6666;
		pub const SQLITE_IOERR_VNODE: i32 = 6922;
		pub const SQLITE_IOERR_AUTH: i32 = 7178;
		pub const SQLITE_LOCKED_SHAREDCACHE: i32 = 262;
		pub const SQLITE_BUSY_RECOVERY: i32 = 261;
		pub const SQLITE_BUSY_SNAPSHOT: i32 = 517;
		pub const SQLITE_CANTOPEN_NOTEMPDIR: i32 = 270;
		pub const SQLITE_CANTOPEN_ISDIR: i32 = 526;
		pub const SQLITE_CANTOPEN_FULLPATH: i32 = 782;
		pub const SQLITE_CANTOPEN_CONVPATH: i32 = 1038;
		pub const SQLITE_CORRUPT_VTAB: i32 = 267;
		pub const SQLITE_READONLY_RECOVERY: i32 = 264;
		pub const SQLITE_READONLY_CANTLOCK: i32 = 520;
		pub const SQLITE_READONLY_ROLLBACK: i32 = 776;
		pub const SQLITE_READONLY_DBMOVED: i32 = 1032;
		pub const SQLITE_ABORT_ROLLBACK: i32 = 516;
		pub const SQLITE_CONSTRAINT_CHECK: i32 = 275;
		pub const SQLITE_CONSTRAINT_COMMITHOOK: i32 = 531;
		pub const SQLITE_CONSTRAINT_FOREIGNKEY: i32 = 787;
		pub const SQLITE_CONSTRAINT_FUNCTION: i32 = 1043;
		pub const SQLITE_CONSTRAINT_NOTNULL: i32 = 1299;
		pub const SQLITE_CONSTRAINT_PRIMARYKEY: i32 = 1555;
		pub const SQLITE_CONSTRAINT_TRIGGER: i32 = 1811;
		pub const SQLITE_CONSTRAINT_UNIQUE: i32 = 2067;
		pub const SQLITE_CONSTRAINT_VTAB: i32 = 2323;
		pub const SQLITE_CONSTRAINT_ROWID: i32 = 2579;
		pub const SQLITE_NOTICE_RECOVER_WAL: i32 = 283;
		pub const SQLITE_NOTICE_RECOVER_ROLLBACK: i32 = 539;
		pub const SQLITE_WARNING_AUTOINDEX: i32 = 284;
		pub const SQLITE_AUTH_USER: i32 = 279;
		pub const SQLITE_OK_LOAD_PERMANENTLY: i32 = 256;
		pub const SQLITE_OPEN_READONLY: i32 = 1;
		pub const SQLITE_OPEN_READWRITE: i32 = 2;
		pub const SQLITE_OPEN_CREATE: i32 = 4;
		pub const SQLITE_OPEN_DELETEONCLOSE: i32 = 8;
		pub const SQLITE_OPEN_EXCLUSIVE: i32 = 16;
		pub const SQLITE_OPEN_AUTOPROXY: i32 = 32;
		pub const SQLITE_OPEN_URI: i32 = 64;
		pub const SQLITE_OPEN_MEMORY: i32 = 128;
		pub const SQLITE_OPEN_MAIN_DB: i32 = 256;
		pub const SQLITE_OPEN_TEMP_DB: i32 = 512;
		pub const SQLITE_OPEN_TRANSIENT_DB: i32 = 1024;
		pub const SQLITE_OPEN_MAIN_JOURNAL: i32 = 2048;
		pub const SQLITE_OPEN_TEMP_JOURNAL: i32 = 4096;
		pub const SQLITE_OPEN_SUBJOURNAL: i32 = 8192;
		pub const SQLITE_OPEN_MASTER_JOURNAL: i32 = 16384;
		pub const SQLITE_OPEN_NOMUTEX: i32 = 32768;
		pub const SQLITE_OPEN_FULLMUTEX: i32 = 65536;
		pub const SQLITE_OPEN_SHAREDCACHE: i32 = 131072;
		pub const SQLITE_OPEN_PRIVATECACHE: i32 = 262144;
		pub const SQLITE_OPEN_WAL: i32 = 524288;
		pub const SQLITE_IOCAP_ATOMIC: i32 = 1;
		pub const SQLITE_IOCAP_ATOMIC512: i32 = 2;
		pub const SQLITE_IOCAP_ATOMIC1K: i32 = 4;
		pub const SQLITE_IOCAP_ATOMIC2K: i32 = 8;
		pub const SQLITE_IOCAP_ATOMIC4K: i32 = 16;
		pub const SQLITE_IOCAP_ATOMIC8K: i32 = 32;
		pub const SQLITE_IOCAP_ATOMIC16K: i32 = 64;
		pub const SQLITE_IOCAP_ATOMIC32K: i32 = 128;
		pub const SQLITE_IOCAP_ATOMIC64K: i32 = 256;
		pub const SQLITE_IOCAP_SAFE_APPEND: i32 = 512;
		pub const SQLITE_IOCAP_SEQUENTIAL: i32 = 1024;
		pub const SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN: i32 = 2048;
		pub const SQLITE_IOCAP_POWERSAFE_OVERWRITE: i32 = 4096;
		pub const SQLITE_IOCAP_IMMUTABLE: i32 = 8192;
		pub const SQLITE_LOCK_NONE: i32 = 0;
		pub const SQLITE_LOCK_SHARED: i32 = 1;
		pub const SQLITE_LOCK_RESERVED: i32 = 2;
		pub const SQLITE_LOCK_PENDING: i32 = 3;
		pub const SQLITE_LOCK_EXCLUSIVE: i32 = 4;
		pub const SQLITE_SYNC_NORMAL: i32 = 2;
		pub const SQLITE_SYNC_FULL: i32 = 3;
		pub const SQLITE_SYNC_DATAONLY: i32 = 16;
		pub const SQLITE_FCNTL_LOCKSTATE: i32 = 1;
		pub const SQLITE_FCNTL_GET_LOCKPROXYFILE: i32 = 2;
		pub const SQLITE_FCNTL_SET_LOCKPROXYFILE: i32 = 3;
		pub const SQLITE_FCNTL_LAST_ERRNO: i32 = 4;
		pub const SQLITE_FCNTL_SIZE_HINT: i32 = 5;
		pub const SQLITE_FCNTL_CHUNK_SIZE: i32 = 6;
		pub const SQLITE_FCNTL_FILE_POINTER: i32 = 7;
		pub const SQLITE_FCNTL_SYNC_OMITTED: i32 = 8;
		pub const SQLITE_FCNTL_WIN32_AV_RETRY: i32 = 9;
		pub const SQLITE_FCNTL_PERSIST_WAL: i32 = 10;
		pub const SQLITE_FCNTL_OVERWRITE: i32 = 11;
		pub const SQLITE_FCNTL_VFSNAME: i32 = 12;
		pub const SQLITE_FCNTL_POWERSAFE_OVERWRITE: i32 = 13;
		pub const SQLITE_FCNTL_PRAGMA: i32 = 14;
		pub const SQLITE_FCNTL_BUSYHANDLER: i32 = 15;
		pub const SQLITE_FCNTL_TEMPFILENAME: i32 = 16;
		pub const SQLITE_FCNTL_MMAP_SIZE: i32 = 18;
		pub const SQLITE_FCNTL_TRACE: i32 = 19;
		pub const SQLITE_FCNTL_HAS_MOVED: i32 = 20;
		pub const SQLITE_FCNTL_SYNC: i32 = 21;
		pub const SQLITE_FCNTL_COMMIT_PHASETWO: i32 = 22;
		pub const SQLITE_FCNTL_WIN32_SET_HANDLE: i32 = 23;
		pub const SQLITE_FCNTL_WAL_BLOCK: i32 = 24;
		pub const SQLITE_FCNTL_ZIPVFS: i32 = 25;
		pub const SQLITE_FCNTL_RBU: i32 = 26;
		pub const SQLITE_FCNTL_VFS_POINTER: i32 = 27;
		pub const SQLITE_FCNTL_JOURNAL_POINTER: i32 = 28;
		pub const SQLITE_GET_LOCKPROXYFILE: i32 = 2;
		pub const SQLITE_SET_LOCKPROXYFILE: i32 = 3;
		pub const SQLITE_LAST_ERRNO: i32 = 4;
		pub const SQLITE_ACCESS_EXISTS: i32 = 0;
		pub const SQLITE_ACCESS_READWRITE: i32 = 1;
		pub const SQLITE_ACCESS_READ: i32 = 2;
		pub const SQLITE_SHM_UNLOCK: i32 = 1;
		pub const SQLITE_SHM_LOCK: i32 = 2;
		pub const SQLITE_SHM_SHARED: i32 = 4;
		pub const SQLITE_SHM_EXCLUSIVE: i32 = 8;
		pub const SQLITE_SHM_NLOCK: i32 = 8;
		pub const SQLITE_CONFIG_SINGLETHREAD: i32 = 1;
		pub const SQLITE_CONFIG_MULTITHREAD: i32 = 2;
		pub const SQLITE_CONFIG_SERIALIZED: i32 = 3;
		pub const SQLITE_CONFIG_MALLOC: i32 = 4;
		pub const SQLITE_CONFIG_GETMALLOC: i32 = 5;
		pub const SQLITE_CONFIG_SCRATCH: i32 = 6;
		pub const SQLITE_CONFIG_PAGECACHE: i32 = 7;
		pub const SQLITE_CONFIG_HEAP: i32 = 8;
		pub const SQLITE_CONFIG_MEMSTATUS: i32 = 9;
		pub const SQLITE_CONFIG_MUTEX: i32 = 10;
		pub const SQLITE_CONFIG_GETMUTEX: i32 = 11;
		pub const SQLITE_CONFIG_LOOKASIDE: i32 = 13;
		pub const SQLITE_CONFIG_PCACHE: i32 = 14;
		pub const SQLITE_CONFIG_GETPCACHE: i32 = 15;
		pub const SQLITE_CONFIG_LOG: i32 = 16;
		pub const SQLITE_CONFIG_URI: i32 = 17;
		pub const SQLITE_CONFIG_PCACHE2: i32 = 18;
		pub const SQLITE_CONFIG_GETPCACHE2: i32 = 19;
		pub const SQLITE_CONFIG_COVERING_INDEX_SCAN: i32 = 20;
		pub const SQLITE_CONFIG_SQLLOG: i32 = 21;
		pub const SQLITE_CONFIG_MMAP_SIZE: i32 = 22;
		pub const SQLITE_CONFIG_WIN32_HEAPSIZE: i32 = 23;
		pub const SQLITE_CONFIG_PCACHE_HDRSZ: i32 = 24;
		pub const SQLITE_CONFIG_PMASZ: i32 = 25;
		pub const SQLITE_CONFIG_STMTJRNL_SPILL: i32 = 26;
		pub const SQLITE_DBCONFIG_LOOKASIDE: i32 = 1001;
		pub const SQLITE_DBCONFIG_ENABLE_FKEY: i32 = 1002;
		pub const SQLITE_DBCONFIG_ENABLE_TRIGGER: i32 = 1003;
		pub const SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER: i32 = 1004;
		pub const SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION: i32 = 1005;
		pub const SQLITE_DENY: i32 = 1;
		pub const SQLITE_IGNORE: i32 = 2;
		pub const SQLITE_CREATE_INDEX: i32 = 1;
		pub const SQLITE_CREATE_TABLE: i32 = 2;
		pub const SQLITE_CREATE_TEMP_INDEX: i32 = 3;
		pub const SQLITE_CREATE_TEMP_TABLE: i32 = 4;
		pub const SQLITE_CREATE_TEMP_TRIGGER: i32 = 5;
		pub const SQLITE_CREATE_TEMP_VIEW: i32 = 6;
		pub const SQLITE_CREATE_TRIGGER: i32 = 7;
		pub const SQLITE_CREATE_VIEW: i32 = 8;
		pub const SQLITE_DELETE: i32 = 9;
		pub const SQLITE_DROP_INDEX: i32 = 10;
		pub const SQLITE_DROP_TABLE: i32 = 11;
		pub const SQLITE_DROP_TEMP_INDEX: i32 = 12;
		pub const SQLITE_DROP_TEMP_TABLE: i32 = 13;
		pub const SQLITE_DROP_TEMP_TRIGGER: i32 = 14;
		pub const SQLITE_DROP_TEMP_VIEW: i32 = 15;
		pub const SQLITE_DROP_TRIGGER: i32 = 16;
		pub const SQLITE_DROP_VIEW: i32 = 17;
		pub const SQLITE_INSERT: i32 = 18;
		pub const SQLITE_PRAGMA: i32 = 19;
		pub const SQLITE_READ: i32 = 20;
		pub const SQLITE_SELECT: i32 = 21;
		pub const SQLITE_TRANSACTION: i32 = 22;
		pub const SQLITE_UPDATE: i32 = 23;
		pub const SQLITE_ATTACH: i32 = 24;
		pub const SQLITE_DETACH: i32 = 25;
		pub const SQLITE_ALTER_TABLE: i32 = 26;
		pub const SQLITE_REINDEX: i32 = 27;
		pub const SQLITE_ANALYZE: i32 = 28;
		pub const SQLITE_CREATE_VTABLE: i32 = 29;
		pub const SQLITE_DROP_VTABLE: i32 = 30;
		pub const SQLITE_FUNCTION: i32 = 31;
		pub const SQLITE_SAVEPOINT: i32 = 32;
		pub const SQLITE_COPY: i32 = 0;
		pub const SQLITE_RECURSIVE: i32 = 33;
		pub const SQLITE_TRACE_STMT: ::os::raw::c_uint = 1;
		pub const SQLITE_TRACE_PROFILE: ::os::raw::c_uint = 2;
		pub const SQLITE_TRACE_ROW: ::os::raw::c_uint = 4;
		pub const SQLITE_TRACE_CLOSE: ::os::raw::c_uint = 8;
		pub const SQLITE_LIMIT_LENGTH: i32 = 0;
		pub const SQLITE_LIMIT_SQL_LENGTH: i32 = 1;
		pub const SQLITE_LIMIT_COLUMN: i32 = 2;
		pub const SQLITE_LIMIT_EXPR_DEPTH: i32 = 3;
		pub const SQLITE_LIMIT_COMPOUND_SELECT: i32 = 4;
		pub const SQLITE_LIMIT_VDBE_OP: i32 = 5;
		pub const SQLITE_LIMIT_FUNCTION_ARG: i32 = 6;
		pub const SQLITE_LIMIT_ATTACHED: i32 = 7;
		pub const SQLITE_LIMIT_LIKE_PATTERN_LENGTH: i32 = 8;
		pub const SQLITE_LIMIT_VARIABLE_NUMBER: i32 = 9;
		pub const SQLITE_LIMIT_TRIGGER_DEPTH: i32 = 10;
		pub const SQLITE_LIMIT_WORKER_THREADS: i32 = 11;
		pub const SQLITE_INTEGER: i32 = 1;
		pub const SQLITE_FLOAT: i32 = 2;
		pub const SQLITE_BLOB: i32 = 4;
		pub const SQLITE_NULL: i32 = 5;
		pub const SQLITE_TEXT: i32 = 3;
		pub const SQLITE3_TEXT: i32 = 3;
		pub const SQLITE_UTF8: i32 = 1;
		pub const SQLITE_UTF16LE: i32 = 2;
		pub const SQLITE_UTF16BE: i32 = 3;
		pub const SQLITE_UTF16: i32 = 4;
		pub const SQLITE_ANY: i32 = 5;
		pub const SQLITE_UTF16_ALIGNED: i32 = 8;
		pub const SQLITE_DETERMINISTIC: i32 = 2048;
		pub const SQLITE_INDEX_SCAN_UNIQUE: i32 = 1;
		pub const SQLITE_INDEX_CONSTRAINT_EQ: i32 = 2;
		pub const SQLITE_INDEX_CONSTRAINT_GT: i32 = 4;
		pub const SQLITE_INDEX_CONSTRAINT_LE: i32 = 8;
		pub const SQLITE_INDEX_CONSTRAINT_LT: i32 = 16;
		pub const SQLITE_INDEX_CONSTRAINT_GE: i32 = 32;
		pub const SQLITE_INDEX_CONSTRAINT_MATCH: i32 = 64;
		pub const SQLITE_INDEX_CONSTRAINT_LIKE: i32 = 65;
		pub const SQLITE_INDEX_CONSTRAINT_GLOB: i32 = 66;
		pub const SQLITE_INDEX_CONSTRAINT_REGEXP: i32 = 67;
		pub const SQLITE_MUTEX_FAST: i32 = 0;
		pub const SQLITE_MUTEX_RECURSIVE: i32 = 1;
		pub const SQLITE_MUTEX_STATIC_MASTER: i32 = 2;
		pub const SQLITE_MUTEX_STATIC_MEM: i32 = 3;
		pub const SQLITE_MUTEX_STATIC_MEM2: i32 = 4;
		pub const SQLITE_MUTEX_STATIC_OPEN: i32 = 4;
		pub const SQLITE_MUTEX_STATIC_PRNG: i32 = 5;
		pub const SQLITE_MUTEX_STATIC_LRU: i32 = 6;
		pub const SQLITE_MUTEX_STATIC_LRU2: i32 = 7;
		pub const SQLITE_MUTEX_STATIC_PMEM: i32 = 7;
		pub const SQLITE_MUTEX_STATIC_APP1: i32 = 8;
		pub const SQLITE_MUTEX_STATIC_APP2: i32 = 9;
		pub const SQLITE_MUTEX_STATIC_APP3: i32 = 10;
		pub const SQLITE_MUTEX_STATIC_VFS1: i32 = 11;
		pub const SQLITE_MUTEX_STATIC_VFS2: i32 = 12;
		pub const SQLITE_MUTEX_STATIC_VFS3: i32 = 13;
		pub const SQLITE_TESTCTRL_FIRST: i32 = 5;
		pub const SQLITE_TESTCTRL_PRNG_SAVE: i32 = 5;
		pub const SQLITE_TESTCTRL_PRNG_RESTORE: i32 = 6;
		pub const SQLITE_TESTCTRL_PRNG_RESET: i32 = 7;
		pub const SQLITE_TESTCTRL_BITVEC_TEST: i32 = 8;
		pub const SQLITE_TESTCTRL_FAULT_INSTALL: i32 = 9;
		pub const SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS: i32 = 10;
		pub const SQLITE_TESTCTRL_PENDING_BYTE: i32 = 11;
		pub const SQLITE_TESTCTRL_ASSERT: i32 = 12;
		pub const SQLITE_TESTCTRL_ALWAYS: i32 = 13;
		pub const SQLITE_TESTCTRL_RESERVE: i32 = 14;
		pub const SQLITE_TESTCTRL_OPTIMIZATIONS: i32 = 15;
		pub const SQLITE_TESTCTRL_ISKEYWORD: i32 = 16;
		pub const SQLITE_TESTCTRL_SCRATCHMALLOC: i32 = 17;
		pub const SQLITE_TESTCTRL_LOCALTIME_FAULT: i32 = 18;
		pub const SQLITE_TESTCTRL_EXPLAIN_STMT: i32 = 19;
		pub const SQLITE_TESTCTRL_NEVER_CORRUPT: i32 = 20;
		pub const SQLITE_TESTCTRL_VDBE_COVERAGE: i32 = 21;
		pub const SQLITE_TESTCTRL_BYTEORDER: i32 = 22;
		pub const SQLITE_TESTCTRL_ISINIT: i32 = 23;
		pub const SQLITE_TESTCTRL_SORTER_MMAP: i32 = 24;
		pub const SQLITE_TESTCTRL_IMPOSTER: i32 = 25;
		pub const SQLITE_TESTCTRL_LAST: i32 = 25;
		pub const SQLITE_STATUS_MEMORY_USED: i32 = 0;
		pub const SQLITE_STATUS_PAGECACHE_USED: i32 = 1;
		pub const SQLITE_STATUS_PAGECACHE_OVERFLOW: i32 = 2;
		pub const SQLITE_STATUS_SCRATCH_USED: i32 = 3;
		pub const SQLITE_STATUS_SCRATCH_OVERFLOW: i32 = 4;
		pub const SQLITE_STATUS_MALLOC_SIZE: i32 = 5;
		pub const SQLITE_STATUS_PARSER_STACK: i32 = 6;
		pub const SQLITE_STATUS_PAGECACHE_SIZE: i32 = 7;
		pub const SQLITE_STATUS_SCRATCH_SIZE: i32 = 8;
		pub const SQLITE_STATUS_MALLOC_COUNT: i32 = 9;
		pub const SQLITE_DBSTATUS_LOOKASIDE_USED: i32 = 0;
		pub const SQLITE_DBSTATUS_CACHE_USED: i32 = 1;
		pub const SQLITE_DBSTATUS_SCHEMA_USED: i32 = 2;
		pub const SQLITE_DBSTATUS_STMT_USED: i32 = 3;
		pub const SQLITE_DBSTATUS_LOOKASIDE_HIT: i32 = 4;
		pub const SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE: i32 = 5;
		pub const SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL: i32 = 6;
		pub const SQLITE_DBSTATUS_CACHE_HIT: i32 = 7;
		pub const SQLITE_DBSTATUS_CACHE_MISS: i32 = 8;
		pub const SQLITE_DBSTATUS_CACHE_WRITE: i32 = 9;
		pub const SQLITE_DBSTATUS_DEFERRED_FKS: i32 = 10;
		pub const SQLITE_DBSTATUS_CACHE_USED_SHARED: i32 = 11;
		pub const SQLITE_DBSTATUS_MAX: i32 = 11;
		pub const SQLITE_STMTSTATUS_FULLSCAN_STEP: i32 = 1;
		pub const SQLITE_STMTSTATUS_SORT: i32 = 2;
		pub const SQLITE_STMTSTATUS_AUTOINDEX: i32 = 3;
		pub const SQLITE_STMTSTATUS_VM_STEP: i32 = 4;
		pub const SQLITE_CHECKPOINT_PASSIVE: i32 = 0;
		pub const SQLITE_CHECKPOINT_FULL: i32 = 1;
		pub const SQLITE_CHECKPOINT_RESTART: i32 = 2;
		pub const SQLITE_CHECKPOINT_TRUNCATE: i32 = 3;
		pub const SQLITE_VTAB_CONSTRAINT_SUPPORT: i32 = 1;
		pub const SQLITE_ROLLBACK: i32 = 1;
		pub const SQLITE_FAIL: i32 = 3;
		pub const SQLITE_REPLACE: i32 = 5;
		pub const SQLITE_SCANSTAT_NLOOP: i32 = 0;
		pub const SQLITE_SCANSTAT_NVISIT: i32 = 1;
		pub const SQLITE_SCANSTAT_EST: i32 = 2;
		pub const SQLITE_SCANSTAT_NAME: i32 = 3;
		pub const SQLITE_SCANSTAT_EXPLAIN: i32 = 4;
		pub const SQLITE_SCANSTAT_SELECTID: i32 = 5;
		pub const NOT_WITHIN: i32 = 0;
		pub const PARTLY_WITHIN: i32 = 1;
		pub const FULLY_WITHIN: i32 = 2;
		pub const SQLITE_CHANGESET_DATA: i32 = 1;
		pub const SQLITE_CHANGESET_NOTFOUND: i32 = 2;
		pub const SQLITE_CHANGESET_CONFLICT: i32 = 3;
		pub const SQLITE_CHANGESET_CONSTRAINT: i32 = 4;
		pub const SQLITE_CHANGESET_FOREIGN_KEY: i32 = 5;
		pub const SQLITE_CHANGESET_OMIT: i32 = 0;
		pub const SQLITE_CHANGESET_REPLACE: i32 = 1;
		pub const SQLITE_CHANGESET_ABORT: i32 = 2;
		pub const FTS5_TOKENIZE_QUERY: i32 = 1;
		pub const FTS5_TOKENIZE_PREFIX: i32 = 2;
		pub const FTS5_TOKENIZE_DOCUMENT: i32 = 4;
		pub const FTS5_TOKENIZE_AUX: i32 = 8;
		pub const FTS5_TOKEN_COLOCATED: i32 = 1;
		unsafe extern "C"
		{
			pub fn sqlite3_auto_extension
			(
				xEntryPoint: ::option::Option<
					unsafe extern "C" fn
                    (	db: *mut sqlite3,
						pzErrMsg: *mut *mut ::os::raw::c_char,
						_: *const sqlite3_api_routines,
					) -> ::os::raw::c_int,
				>,
			) -> ::os::raw::c_int;
			
			pub fn sqlite3_cancel_auto_extension
			(
				xEntryPoint: ::option::Option<
					unsafe extern "C" fn
                    (
						db: *mut sqlite3,
						pzErrMsg: *mut *mut ::os::raw::c_char,
						_: *const sqlite3_api_routines,
					) -> ::os::raw::c_int,
				>,
			) -> ::os::raw::c_int;
			pub static sqlite3_version: [::os::raw::c_char; 0usize];
			pub fn sqlite3_libversion() -> *const ::os::raw::c_char;
			pub fn sqlite3_sourceid() -> *const ::os::raw::c_char;
			pub fn sqlite3_libversion_number() -> ::os::raw::c_int;
			pub fn sqlite3_compileoption_used( zOptName: *const ::os::raw::c_char ) -> ::os::raw::c_int;
			pub fn sqlite3_compileoption_get(N: ::os::raw::c_int) -> *const ::os::raw::c_char;
			pub fn sqlite3_threadsafe() -> ::os::raw::c_int;
			pub fn sqlite3_close(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_exec(
				arg1: *mut sqlite3,
				sql: *const ::os::raw::c_char,
				callback: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: ::os::raw::c_int,
						arg3: *mut *mut ::os::raw::c_char,
						arg4: *mut *mut ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				arg2: *mut ::os::raw::c_void,
				errmsg: *mut *mut ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_initialize() -> ::os::raw::c_int;
			pub fn sqlite3_shutdown() -> ::os::raw::c_int;
			pub fn sqlite3_os_init() -> ::os::raw::c_int;
			pub fn sqlite3_os_end() -> ::os::raw::c_int;
			pub fn sqlite3_config(arg1: ::os::raw::c_int, ...) -> ::os::raw::c_int;
			pub fn sqlite3_db_config(
				arg1: *mut sqlite3,
				op: ::os::raw::c_int,
				...
			) -> ::os::raw::c_int;
			pub fn sqlite3_extended_result_codes(
				arg1: *mut sqlite3,
				onoff: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_last_insert_rowid(arg1: *mut sqlite3) -> sqlite3_int64;
			pub fn sqlite3_changes(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_total_changes(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_interrupt(arg1: *mut sqlite3);
			pub fn sqlite3_complete(sql: *const ::os::raw::c_char) -> ::os::raw::c_int;
			pub fn sqlite3_busy_handler(
				arg1: *mut sqlite3,
				arg2: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				arg3: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_busy_timeout(
				arg1: *mut sqlite3,
				ms: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_get_table(
				db: *mut sqlite3,
				zSql: *const ::os::raw::c_char,
				pazResult: *mut *mut *mut ::os::raw::c_char,
				pnRow: *mut ::os::raw::c_int,
				pnColumn: *mut ::os::raw::c_int,
				pzErrmsg: *mut *mut ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_free_table(result: *mut *mut ::os::raw::c_char);
			pub fn sqlite3_mprintf(arg1: *const ::os::raw::c_char, ...)
				-> *mut ::os::raw::c_char;
			pub fn sqlite3_snprintf(
				arg1: ::os::raw::c_int,
				arg2: *mut ::os::raw::c_char,
				arg3: *const ::os::raw::c_char,
				...
			) -> *mut ::os::raw::c_char;
			pub fn sqlite3_malloc(arg1: ::os::raw::c_int) -> *mut ::os::raw::c_void;
			pub fn sqlite3_malloc64(arg1: sqlite3_uint64) -> *mut ::os::raw::c_void;
			pub fn sqlite3_realloc(
				arg1: *mut ::os::raw::c_void,
				arg2: ::os::raw::c_int,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_realloc64(
				arg1: *mut ::os::raw::c_void,
				arg2: sqlite3_uint64,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_free(arg1: *mut ::os::raw::c_void);
			pub fn sqlite3_msize(arg1: *mut ::os::raw::c_void) -> sqlite3_uint64;
			pub fn sqlite3_memory_used() -> sqlite3_int64;
			pub fn sqlite3_memory_highwater(resetFlag: ::os::raw::c_int) -> sqlite3_int64;
			pub fn sqlite3_randomness(N: ::os::raw::c_int, P: *mut ::os::raw::c_void);
			pub fn sqlite3_set_authorizer(
				arg1: *mut sqlite3,
				xAuth: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: ::os::raw::c_int,
						arg3: *const ::os::raw::c_char,
						arg4: *const ::os::raw::c_char,
						arg5: *const ::os::raw::c_char,
						arg6: *const ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				pUserData: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_trace(
				arg1: *mut sqlite3,
				xTrace: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: *const ::os::raw::c_char,
					),
				>,
				arg2: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_profile(
				arg1: *mut sqlite3,
				xProfile: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: *const ::os::raw::c_char,
						arg3: sqlite3_uint64,
					),
				>,
				arg2: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_trace_v2(
				arg1: *mut sqlite3,
				uMask: ::os::raw::c_uint,
				xCallback: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: ::os::raw::c_uint,
						arg2: *mut ::os::raw::c_void,
						arg3: *mut ::os::raw::c_void,
						arg4: *mut ::os::raw::c_void,
					) -> ::os::raw::c_int,
				>,
				pCtx: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_progress_handler(
				arg1: *mut sqlite3,
				arg2: ::os::raw::c_int,
				arg3: ::option::Option<
					unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
				>,
				arg4: *mut ::os::raw::c_void,
			);
			pub fn sqlite3_open(
				filename: *const ::os::raw::c_char,
				ppDb: *mut *mut sqlite3,
			) -> ::os::raw::c_int;
			pub fn sqlite3_open_v2(
				filename: *const ::os::raw::c_char,
				ppDb: *mut *mut sqlite3,
				flags: ::os::raw::c_int,
				zVfs: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_uri_parameter(
				zFilename: *const ::os::raw::c_char,
				zParam: *const ::os::raw::c_char,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_uri_boolean(
				zFile: *const ::os::raw::c_char,
				zParam: *const ::os::raw::c_char,
				bDefault: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_uri_int64(
				arg1: *const ::os::raw::c_char,
				arg2: *const ::os::raw::c_char,
				arg3: sqlite3_int64,
			) -> sqlite3_int64;
			pub fn sqlite3_errcode(db: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_extended_errcode(db: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_errmsg(arg1: *mut sqlite3) -> *const ::os::raw::c_char;
			pub fn sqlite3_errstr(arg1: ::os::raw::c_int) -> *const ::os::raw::c_char;
			pub fn sqlite3_limit(
				arg1: *mut sqlite3,
				id: ::os::raw::c_int,
				newVal: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_prepare_v2(
				db: *mut sqlite3,
				zSql: *const ::os::raw::c_char,
				nByte: ::os::raw::c_int,
				ppStmt: *mut *mut sqlite3_stmt,
				pzTail: *mut *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_sql(pStmt: *mut sqlite3_stmt) -> *const ::os::raw::c_char;
			pub fn sqlite3_expanded_sql(pStmt: *mut sqlite3_stmt) -> *mut ::os::raw::c_char;
			pub fn sqlite3_stmt_readonly(pStmt: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_stmt_busy(arg1: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_bind_blob(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: *const ::os::raw::c_void,
				n: ::os::raw::c_int,
				arg4: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_blob64(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: *const ::os::raw::c_void,
				arg4: sqlite3_uint64,
				arg5: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_double(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: f64,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_int(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_int64(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: sqlite3_int64,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_null(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_text(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: *const ::os::raw::c_char,
				arg4: ::os::raw::c_int,
				arg5: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_text64(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: *const ::os::raw::c_char,
				arg4: sqlite3_uint64,
				arg5: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				encoding: ::os::raw::c_uchar,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_value(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: *const sqlite3_value,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_zeroblob(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				n: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_zeroblob64(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
				arg3: sqlite3_uint64,
			) -> ::os::raw::c_int;
			pub fn sqlite3_bind_parameter_count(arg1: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_bind_parameter_name(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_bind_parameter_index(
				arg1: *mut sqlite3_stmt,
				zName: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_clear_bindings(arg1: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_column_count(pStmt: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_column_name(
				arg1: *mut sqlite3_stmt,
				N: ::os::raw::c_int,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_column_database_name(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_column_table_name(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_column_origin_name(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_column_decltype(
				arg1: *mut sqlite3_stmt,
				arg2: ::os::raw::c_int,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_step(arg1: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_data_count(pStmt: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_column_blob(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> *const ::os::raw::c_void;
			pub fn sqlite3_column_bytes(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_column_double(arg1: *mut sqlite3_stmt, iCol: ::os::raw::c_int) -> f64;
			pub fn sqlite3_column_int(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_column_int64(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> sqlite3_int64;
			pub fn sqlite3_column_text(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> *const ::os::raw::c_uchar;
			pub fn sqlite3_column_type(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_column_value(
				arg1: *mut sqlite3_stmt,
				iCol: ::os::raw::c_int,
			) -> *mut sqlite3_value;
			pub fn sqlite3_finalize(pStmt: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_reset(pStmt: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_create_function_v2(
				db: *mut sqlite3,
				zFunctionName: *const ::os::raw::c_char,
				nArg: ::os::raw::c_int,
				eTextRep: ::os::raw::c_int,
				pApp: *mut ::os::raw::c_void,
				xFunc: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut sqlite3_context,
						arg2: ::os::raw::c_int,
						arg3: *mut *mut sqlite3_value,
					),
				>,
				xStep: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut sqlite3_context,
						arg2: ::os::raw::c_int,
						arg3: *mut *mut sqlite3_value,
					),
				>,
				xFinal: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_context)>,
				xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_aggregate_count(arg1: *mut sqlite3_context) -> ::os::raw::c_int;
			pub fn sqlite3_expired(arg1: *mut sqlite3_stmt) -> ::os::raw::c_int;
			pub fn sqlite3_transfer_bindings(
				arg1: *mut sqlite3_stmt,
				arg2: *mut sqlite3_stmt,
			) -> ::os::raw::c_int;
			pub fn sqlite3_global_recover() -> ::os::raw::c_int;
			pub fn sqlite3_thread_cleanup();
			pub fn sqlite3_memory_alarm(
				arg1: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: sqlite3_int64,
						arg3: ::os::raw::c_int,
					),
				>,
				arg2: *mut ::os::raw::c_void,
				arg3: sqlite3_int64,
			) -> ::os::raw::c_int;
			pub fn sqlite3_value_blob(arg1: *mut sqlite3_value) -> *const ::os::raw::c_void;
			pub fn sqlite3_value_bytes(arg1: *mut sqlite3_value) -> ::os::raw::c_int;
			pub fn sqlite3_value_double(arg1: *mut sqlite3_value) -> f64;
			pub fn sqlite3_value_int(arg1: *mut sqlite3_value) -> ::os::raw::c_int;
			pub fn sqlite3_value_int64(arg1: *mut sqlite3_value) -> sqlite3_int64;
			pub fn sqlite3_value_text(arg1: *mut sqlite3_value) -> *const ::os::raw::c_uchar;
			pub fn sqlite3_value_type(arg1: *mut sqlite3_value) -> ::os::raw::c_int;
			pub fn sqlite3_value_numeric_type(arg1: *mut sqlite3_value) -> ::os::raw::c_int;
			pub fn sqlite3_value_subtype(arg1: *mut sqlite3_value) -> ::os::raw::c_uint;
			pub fn sqlite3_value_dup(arg1: *const sqlite3_value) -> *mut sqlite3_value;
			pub fn sqlite3_value_free(arg1: *mut sqlite3_value);
			pub fn sqlite3_aggregate_context(
				arg1: *mut sqlite3_context,
				nBytes: ::os::raw::c_int,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_user_data(arg1: *mut sqlite3_context) -> *mut ::os::raw::c_void;
			pub fn sqlite3_context_db_handle(arg1: *mut sqlite3_context) -> *mut sqlite3;
			pub fn sqlite3_get_auxdata(
				arg1: *mut sqlite3_context,
				N: ::os::raw::c_int,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_set_auxdata(
				arg1: *mut sqlite3_context,
				N: ::os::raw::c_int,
				arg2: *mut ::os::raw::c_void,
				arg3: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			);
			pub fn sqlite3_result_blob(
				arg1: *mut sqlite3_context,
				arg2: *const ::os::raw::c_void,
				arg3: ::os::raw::c_int,
				arg4: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			);
			pub fn sqlite3_result_blob64(
				arg1: *mut sqlite3_context,
				arg2: *const ::os::raw::c_void,
				arg3: sqlite3_uint64,
				arg4: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			);
			pub fn sqlite3_result_double(arg1: *mut sqlite3_context, arg2: f64);
			pub fn sqlite3_result_error(
				arg1: *mut sqlite3_context,
				arg2: *const ::os::raw::c_char,
				arg3: ::os::raw::c_int,
			);
			pub fn sqlite3_result_error_toobig(arg1: *mut sqlite3_context);
			pub fn sqlite3_result_error_nomem(arg1: *mut sqlite3_context);
			pub fn sqlite3_result_error_code(arg1: *mut sqlite3_context, arg2: ::os::raw::c_int);
			pub fn sqlite3_result_int(arg1: *mut sqlite3_context, arg2: ::os::raw::c_int);
			pub fn sqlite3_result_int64(arg1: *mut sqlite3_context, arg2: sqlite3_int64);
			pub fn sqlite3_result_null(arg1: *mut sqlite3_context);
			pub fn sqlite3_result_text(
				arg1: *mut sqlite3_context,
				arg2: *const ::os::raw::c_char,
				arg3: ::os::raw::c_int,
				arg4: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			);
			pub fn sqlite3_result_text64(
				arg1: *mut sqlite3_context,
				arg2: *const ::os::raw::c_char,
				arg3: sqlite3_uint64,
				arg4: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				encoding: ::os::raw::c_uchar,
			);
			pub fn sqlite3_result_value(arg1: *mut sqlite3_context, arg2: *mut sqlite3_value);
			pub fn sqlite3_result_zeroblob(arg1: *mut sqlite3_context, n: ::os::raw::c_int);
			pub fn sqlite3_result_zeroblob64(
				arg1: *mut sqlite3_context,
				n: sqlite3_uint64,
			) -> ::os::raw::c_int;
			pub fn sqlite3_result_subtype(arg1: *mut sqlite3_context, arg2: ::os::raw::c_uint);
			pub fn sqlite3_create_collation_v2(
				arg1: *mut sqlite3,
				zName: *const ::os::raw::c_char,
				eTextRep: ::os::raw::c_int,
				pArg: *mut ::os::raw::c_void,
				xCompare: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: ::os::raw::c_int,
						arg3: *const ::os::raw::c_void,
						arg4: ::os::raw::c_int,
						arg5: *const ::os::raw::c_void,
					) -> ::os::raw::c_int,
				>,
				xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_collation_needed(
				arg1: *mut sqlite3,
				arg2: *mut ::os::raw::c_void,
				arg3: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: *mut sqlite3,
						eTextRep: ::os::raw::c_int,
						arg3: *const ::os::raw::c_char,
					),
				>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_sleep(arg1: ::os::raw::c_int) -> ::os::raw::c_int;
			pub static mut sqlite3_temp_directory: *mut ::os::raw::c_char;
			pub static mut sqlite3_data_directory: *mut ::os::raw::c_char;
			pub fn sqlite3_get_autocommit(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_db_handle(arg1: *mut sqlite3_stmt) -> *mut sqlite3;
			pub fn sqlite3_db_filename(
				db: *mut sqlite3,
				zDbName: *const ::os::raw::c_char,
			) -> *const ::os::raw::c_char;
			pub fn sqlite3_db_readonly(
				db: *mut sqlite3,
				zDbName: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_next_stmt(pDb: *mut sqlite3, pStmt: *mut sqlite3_stmt) -> *mut sqlite3_stmt;
			pub fn sqlite3_commit_hook(
				arg1: *mut sqlite3,
				arg2: ::option::Option<
					unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
				>,
				arg3: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_rollback_hook(
				arg1: *mut sqlite3,
				arg2: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				arg3: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_update_hook(
				arg1: *mut sqlite3,
				arg2: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: ::os::raw::c_int,
						arg3: *const ::os::raw::c_char,
						arg4: *const ::os::raw::c_char,
						arg5: sqlite3_int64,
					),
				>,
				arg3: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_enable_shared_cache(arg1: ::os::raw::c_int) -> ::os::raw::c_int;
			pub fn sqlite3_release_memory(arg1: ::os::raw::c_int) -> ::os::raw::c_int;
			pub fn sqlite3_db_release_memory(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_soft_heap_limit64(N: sqlite3_int64) -> sqlite3_int64;
			pub fn sqlite3_soft_heap_limit(N: ::os::raw::c_int);
			pub fn sqlite3_table_column_metadata(
				db: *mut sqlite3,
				zDbName: *const ::os::raw::c_char,
				zTableName: *const ::os::raw::c_char,
				zColumnName: *const ::os::raw::c_char,
				pzDataType: *mut *const ::os::raw::c_char,
				pzCollSeq: *mut *const ::os::raw::c_char,
				pNotNull: *mut ::os::raw::c_int,
				pPrimaryKey: *mut ::os::raw::c_int,
				pAutoinc: *mut ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_load_extension(
				db: *mut sqlite3,
				zFile: *const ::os::raw::c_char,
				zProc: *const ::os::raw::c_char,
				pzErrMsg: *mut *mut ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_enable_load_extension(
				db: *mut sqlite3,
				onoff: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_reset_auto_extension();
			pub fn sqlite3_create_module_v2(
				db: *mut sqlite3,
				zName: *const ::os::raw::c_char,
				p: *const sqlite3_module,
				pClientData: *mut ::os::raw::c_void,
				xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3_declare_vtab(
				arg1: *mut sqlite3,
				zSQL: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_overload_function(
				arg1: *mut sqlite3,
				zFuncName: *const ::os::raw::c_char,
				nArg: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_blob_open(
				arg1: *mut sqlite3,
				zDb: *const ::os::raw::c_char,
				zTable: *const ::os::raw::c_char,
				zColumn: *const ::os::raw::c_char,
				iRow: sqlite3_int64,
				flags: ::os::raw::c_int,
				ppBlob: *mut *mut sqlite3_blob,
			) -> ::os::raw::c_int;
			pub fn sqlite3_blob_reopen(
				arg1: *mut sqlite3_blob,
				arg2: sqlite3_int64,
			) -> ::os::raw::c_int;
			pub fn sqlite3_blob_close(arg1: *mut sqlite3_blob) -> ::os::raw::c_int;
			pub fn sqlite3_blob_bytes(arg1: *mut sqlite3_blob) -> ::os::raw::c_int;
			pub fn sqlite3_blob_read(
				arg1: *mut sqlite3_blob,
				Z: *mut ::os::raw::c_void,
				N: ::os::raw::c_int,
				iOffset: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_blob_write(
				arg1: *mut sqlite3_blob,
				z: *const ::os::raw::c_void,
				n: ::os::raw::c_int,
				iOffset: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_vfs_find(zVfsName: *const ::os::raw::c_char) -> *mut sqlite3_vfs;
			pub fn sqlite3_vfs_register(
				arg1: *mut sqlite3_vfs,
				makeDflt: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_vfs_unregister(arg1: *mut sqlite3_vfs) -> ::os::raw::c_int;
			pub fn sqlite3_mutex_alloc(arg1: ::os::raw::c_int) -> *mut sqlite3_mutex;
			pub fn sqlite3_mutex_free(arg1: *mut sqlite3_mutex);
			pub fn sqlite3_mutex_enter(arg1: *mut sqlite3_mutex);
			pub fn sqlite3_mutex_try(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int;
			pub fn sqlite3_mutex_leave(arg1: *mut sqlite3_mutex);
			pub fn sqlite3_mutex_held(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int;
			pub fn sqlite3_mutex_notheld(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int;
			pub fn sqlite3_db_mutex(arg1: *mut sqlite3) -> *mut sqlite3_mutex;
			pub fn sqlite3_file_control(
				arg1: *mut sqlite3,
				zDbName: *const ::os::raw::c_char,
				op: ::os::raw::c_int,
				arg2: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_test_control(op: ::os::raw::c_int, ...) -> ::os::raw::c_int;
			pub fn sqlite3_status(
				op: ::os::raw::c_int,
				pCurrent: *mut ::os::raw::c_int,
				pHighwater: *mut ::os::raw::c_int,
				resetFlag: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_status64(
				op: ::os::raw::c_int,
				pCurrent: *mut sqlite3_int64,
				pHighwater: *mut sqlite3_int64,
				resetFlag: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_db_status(
				arg1: *mut sqlite3,
				op: ::os::raw::c_int,
				pCur: *mut ::os::raw::c_int,
				pHiwtr: *mut ::os::raw::c_int,
				resetFlg: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_stmt_status(
				arg1: *mut sqlite3_stmt,
				op: ::os::raw::c_int,
				resetFlg: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_backup_init(
				pDest: *mut sqlite3,
				zDestName: *const ::os::raw::c_char,
				pSource: *mut sqlite3,
				zSourceName: *const ::os::raw::c_char,
			) -> *mut sqlite3_backup;
			pub fn sqlite3_backup_step(
				p: *mut sqlite3_backup,
				nPage: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_backup_finish(p: *mut sqlite3_backup) -> ::os::raw::c_int;
			pub fn sqlite3_backup_remaining(p: *mut sqlite3_backup) -> ::os::raw::c_int;
			pub fn sqlite3_backup_pagecount(p: *mut sqlite3_backup) -> ::os::raw::c_int;
			pub fn sqlite3_unlock_notify(
				pBlocked: *mut sqlite3,
				xNotify: ::option::Option<
					unsafe extern "C" fn
                    (	apArg: *mut *mut ::os::raw::c_void,
						nArg: ::os::raw::c_int,
					),
				>,
				pNotifyArg: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_stricmp(
				arg1: *const ::os::raw::c_char,
				arg2: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_strnicmp(
				arg1: *const ::os::raw::c_char,
				arg2: *const ::os::raw::c_char,
				arg3: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_strglob(
				zGlob: *const ::os::raw::c_char,
				zStr: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_strlike(
				zGlob: *const ::os::raw::c_char,
				zStr: *const ::os::raw::c_char,
				cEsc: ::os::raw::c_uint,
			) -> ::os::raw::c_int;
			pub fn sqlite3_log(
				iErrCode: ::os::raw::c_int,
				zFormat: *const ::os::raw::c_char,
				...
			);
			pub fn sqlite3_wal_hook(
				arg1: *mut sqlite3,
				arg2: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut ::os::raw::c_void,
						arg2: *mut sqlite3,
						arg3: *const ::os::raw::c_char,
						arg4: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				arg3: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_wal_autocheckpoint(
				db: *mut sqlite3,
				N: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_wal_checkpoint(
				db: *mut sqlite3,
				zDb: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3_wal_checkpoint_v2(
				db: *mut sqlite3,
				zDb: *const ::os::raw::c_char,
				eMode: ::os::raw::c_int,
				pnLog: *mut ::os::raw::c_int,
				pnCkpt: *mut ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3_vtab_config(
				arg1: *mut sqlite3,
				op: ::os::raw::c_int,
				...
			) -> ::os::raw::c_int;
			pub fn sqlite3_vtab_on_conflict(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_stmt_scanstatus(
				pStmt: *mut sqlite3_stmt,
				idx: ::os::raw::c_int,
				iScanStatusOp: ::os::raw::c_int,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_stmt_scanstatus_reset(arg1: *mut sqlite3_stmt);
			pub fn sqlite3_db_cacheflush(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_preupdate_hook(
				db: *mut sqlite3,
				xPreUpdate: ::option::Option<
					unsafe extern "C" fn
                    (	pCtx: *mut ::os::raw::c_void,
						db: *mut sqlite3,
						op: ::os::raw::c_int,
						zDb: *const ::os::raw::c_char,
						zName: *const ::os::raw::c_char,
						iKey1: sqlite3_int64,
						iKey2: sqlite3_int64,
					),
				>,
				arg1: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_preupdate_old(
				arg1: *mut sqlite3,
				arg2: ::os::raw::c_int,
				arg3: *mut *mut sqlite3_value,
			) -> ::os::raw::c_int;
			pub fn sqlite3_preupdate_count(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_preupdate_depth(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_preupdate_new(
				arg1: *mut sqlite3,
				arg2: ::os::raw::c_int,
				arg3: *mut *mut sqlite3_value,
			) -> ::os::raw::c_int;
			pub fn sqlite3_system_errno(arg1: *mut sqlite3) -> ::os::raw::c_int;
			pub fn sqlite3_snapshot_get(
				db: *mut sqlite3,
				zSchema: *const ::os::raw::c_char,
				ppSnapshot: *mut *mut sqlite3_snapshot,
			) -> ::os::raw::c_int;
			pub fn sqlite3_snapshot_open(
				db: *mut sqlite3,
				zSchema: *const ::os::raw::c_char,
				pSnapshot: *mut sqlite3_snapshot,
			) -> ::os::raw::c_int;
			pub fn sqlite3_snapshot_free(arg1: *mut sqlite3_snapshot);
			pub fn sqlite3_snapshot_cmp(
				p1: *mut sqlite3_snapshot,
				p2: *mut sqlite3_snapshot,
			) -> ::os::raw::c_int;
			pub fn sqlite3_rtree_geometry_callback(
				db: *mut sqlite3,
				zGeom: *const ::os::raw::c_char,
				xGeom: ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut sqlite3_rtree_geometry,
						arg2: ::os::raw::c_int,
						arg3: *mut sqlite3_rtree_dbl,
						arg4: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pContext: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3_rtree_query_callback(
				db: *mut sqlite3,
				zQueryFunc: *const ::os::raw::c_char,
				xQueryFunc: ::option::Option<
					unsafe extern "C" fn(arg1: *mut sqlite3_rtree_query_info) -> ::os::raw::c_int,
				>,
				pContext: *mut ::os::raw::c_void,
				xDestructor: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_create(
				db: *mut sqlite3,
				zDb: *const ::os::raw::c_char,
				ppSession: *mut *mut sqlite3_session,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_delete(pSession: *mut sqlite3_session);
			pub fn sqlite3session_enable(
				pSession: *mut sqlite3_session,
				bEnable: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_indirect(
				pSession: *mut sqlite3_session,
				bIndirect: ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_attach(
				pSession: *mut sqlite3_session,
				zTab: *const ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_table_filter(
				pSession: *mut sqlite3_session,
				xFilter: ::option::Option<
					unsafe extern "C" fn
                    (	pCtx: *mut ::os::raw::c_void,
						zTab: *const ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				pCtx: *mut ::os::raw::c_void,
			);
			pub fn sqlite3session_changeset(
				pSession: *mut sqlite3_session,
				pnChangeset: *mut ::os::raw::c_int,
				ppChangeset: *mut *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_diff(
				pSession: *mut sqlite3_session,
				zFromDb: *const ::os::raw::c_char,
				zTbl: *const ::os::raw::c_char,
				pzErrMsg: *mut *mut ::os::raw::c_char,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_patchset(
				pSession: *mut sqlite3_session,
				pnPatchset: *mut ::os::raw::c_int,
				ppPatchset: *mut *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_isempty(pSession: *mut sqlite3_session) -> ::os::raw::c_int;
			pub fn sqlite3changeset_start(
				pp: *mut *mut sqlite3_changeset_iter,
				nChangeset: ::os::raw::c_int,
				pChangeset: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_next(pIter: *mut sqlite3_changeset_iter) -> ::os::raw::c_int;
			pub fn sqlite3changeset_op(
				pIter: *mut sqlite3_changeset_iter,
				pzTab: *mut *const ::os::raw::c_char,
				pnCol: *mut ::os::raw::c_int,
				pOp: *mut ::os::raw::c_int,
				pbIndirect: *mut ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_pk(
				pIter: *mut sqlite3_changeset_iter,
				pabPK: *mut *mut ::os::raw::c_uchar,
				pnCol: *mut ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_old(
				pIter: *mut sqlite3_changeset_iter,
				iVal: ::os::raw::c_int,
				ppValue: *mut *mut sqlite3_value,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_new(
				pIter: *mut sqlite3_changeset_iter,
				iVal: ::os::raw::c_int,
				ppValue: *mut *mut sqlite3_value,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_conflict(
				pIter: *mut sqlite3_changeset_iter,
				iVal: ::os::raw::c_int,
				ppValue: *mut *mut sqlite3_value,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_fk_conflicts(
				pIter: *mut sqlite3_changeset_iter,
				pnOut: *mut ::os::raw::c_int,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_finalize(pIter: *mut sqlite3_changeset_iter) -> ::os::raw::c_int;
			pub fn sqlite3changeset_invert(
				nIn: ::os::raw::c_int,
				pIn: *const ::os::raw::c_void,
				pnOut: *mut ::os::raw::c_int,
				ppOut: *mut *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_concat(
				nA: ::os::raw::c_int,
				pA: *mut ::os::raw::c_void,
				nB: ::os::raw::c_int,
				pB: *mut ::os::raw::c_void,
				pnOut: *mut ::os::raw::c_int,
				ppOut: *mut *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changegroup_new(pp: *mut *mut sqlite3_changegroup) -> ::os::raw::c_int;
			pub fn sqlite3changegroup_add(
				arg1: *mut sqlite3_changegroup,
				nData: ::os::raw::c_int,
				pData: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changegroup_output(
				arg1: *mut sqlite3_changegroup,
				pnData: *mut ::os::raw::c_int,
				ppData: *mut *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changegroup_delete(arg1: *mut sqlite3_changegroup);
			pub fn sqlite3changeset_apply(
				db: *mut sqlite3,
				nChangeset: ::os::raw::c_int,
				pChangeset: *mut ::os::raw::c_void,
				xFilter: ::option::Option<
					unsafe extern "C" fn
                    (	pCtx: *mut ::os::raw::c_void,
						zTab: *const ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				xConflict: ::option::Option<
					unsafe extern "C" fn
                    (	pCtx: *mut ::os::raw::c_void,
						eConflict: ::os::raw::c_int,
						p: *mut sqlite3_changeset_iter,
					) -> ::os::raw::c_int,
				>,
				pCtx: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_apply_strm(
				db: *mut sqlite3,
				xInput: ::option::Option<
					unsafe extern "C" fn
                    (	pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
				xFilter: ::option::Option<
					unsafe extern "C" fn
                    (	pCtx: *mut ::os::raw::c_void,
						zTab: *const ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				xConflict: ::option::Option<
					unsafe extern "C" fn
                    (	pCtx: *mut ::os::raw::c_void,
						eConflict: ::os::raw::c_int,
						p: *mut sqlite3_changeset_iter,
					) -> ::os::raw::c_int,
				>,
				pCtx: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_concat_strm(
				xInputA: ::option::Option<
					unsafe extern "C" fn
                    (	pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pInA: *mut ::os::raw::c_void,
				xInputB: ::option::Option<
					unsafe extern "C" fn
                    (	pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pInB: *mut ::os::raw::c_void,
				xOutput: ::option::Option<
					unsafe extern "C" fn
                    (	pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_invert_strm(
				xInput: ::option::Option<
					unsafe extern "C" fn
                    (	pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
				xOutput: ::option::Option<
					unsafe extern "C" fn
                    (	pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_start_strm(
				pp: *mut *mut sqlite3_changeset_iter,
				xInput: ::option::Option<
					unsafe extern "C" fn
                    (	pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_changeset_strm(
				pSession: *mut sqlite3_session,
				xOutput: ::option::Option<
					unsafe extern "C" fn
                    (	pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_patchset_strm
			(
				pSession: *mut sqlite3_session,
				xOutput: ::option::Option<
					unsafe extern "C" fn
                    (	pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changegroup_add_strm
			(
				arg1: *mut sqlite3_changegroup,
				xInput: ::option::Option<
					unsafe extern "C" fn
                    (	pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changegroup_output_strm
			(
				arg1: *mut sqlite3_changegroup,
				xOutput: ::option::Option<
					unsafe extern "C" fn
                    (	pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3
		{
			_unused: [u8; 0],
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_file
		{
			pub pMethods: *const sqlite3_io_methods,
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_io_methods
		{
			pub iVersion: ::os::raw::c_int,
			pub xClose: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::os::raw::c_int,
			>,
			pub xRead: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					arg2: *mut ::os::raw::c_void,
					iAmt: ::os::raw::c_int,
					iOfst: sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xWrite: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					arg2: *const ::os::raw::c_void,
					iAmt: ::os::raw::c_int,
					iOfst: sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xTruncate: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file, size: sqlite3_int64) -> ::os::raw::c_int,
			>,
			pub xSync: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					flags: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFileSize: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					pSize: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xLock: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					arg2: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xUnlock: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					arg2: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xCheckReservedLock: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					pResOut: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFileControl: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					op: ::os::raw::c_int,
					pArg: *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xSectorSize: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::os::raw::c_int,
			>,
			pub xDeviceCharacteristics: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::os::raw::c_int,
			>,
			pub xShmMap: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					iPg: ::os::raw::c_int,
					pgsz: ::os::raw::c_int,
					arg2: ::os::raw::c_int,
					arg3: *mut *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xShmLock: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					offset: ::os::raw::c_int,
					n: ::os::raw::c_int,
					flags: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xShmBarrier: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file)>,
			pub xShmUnmap: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					deleteFlag: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFetch: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					iOfst: sqlite3_int64,
					iAmt: ::os::raw::c_int,
					pp: *mut *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xUnfetch: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_file,
					iOfst: sqlite3_int64,
					p: *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_mutex
		{
			_unused: [u8; 0],
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_api_routines
		{
			_unused: [u8; 0],
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_vfs
		{
			pub iVersion: ::os::raw::c_int,
			pub szOsFile: ::os::raw::c_int,
			pub mxPathname: ::os::raw::c_int,
			pub pNext: *mut sqlite3_vfs,
			pub zName: *const ::os::raw::c_char,
			pub pAppData: *mut ::os::raw::c_void,
			pub xOpen: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					arg2: *mut sqlite3_file,
					flags: ::os::raw::c_int,
					pOutFlags: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xDelete: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					syncDir: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xAccess: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					flags: ::os::raw::c_int,
					pResOut: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFullPathname: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					nOut: ::os::raw::c_int,
					zOut: *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xDlOpen: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zFilename: *const ::os::raw::c_char,
				) -> *mut ::os::raw::c_void,
			>,
			pub xDlError: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					nByte: ::os::raw::c_int,
					zErrMsg: *mut ::os::raw::c_char,
				),
			>,
			pub xDlSym: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					arg2: *mut ::os::raw::c_void,
					zSymbol: *const ::os::raw::c_char,
				) -> ::option::Option<
					unsafe extern "C" fn
                    (	arg1: *mut sqlite3_vfs,
						arg2: *mut ::os::raw::c_void,
						zSymbol: *const ::os::raw::c_char,
					),
				>,
			>,
			pub xDlClose: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_vfs, arg2: *mut ::os::raw::c_void),
			>,
			pub xRandomness: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					nByte: ::os::raw::c_int,
					zOut: *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xSleep: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					microseconds: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xCurrentTime: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_vfs, arg2: *mut f64) -> ::os::raw::c_int,
			>,
			pub xGetLastError: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					arg2: ::os::raw::c_int,
					arg3: *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xCurrentTimeInt64: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					arg2: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xSetSystemCall: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					arg2: sqlite3_syscall_ptr,
				) -> ::os::raw::c_int,
			>,
			pub xGetSystemCall: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
				) -> sqlite3_syscall_ptr,
			>,
			pub xNextSystemCall: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
				) -> *const ::os::raw::c_char,
			>,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_mem_methods
		{
			pub xMalloc: ::option::Option<unsafe extern "C" fn(arg1: ::os::raw::c_int) -> *mut ::os::raw::c_void,
			>,
			pub xFree: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub xRealloc: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut ::os::raw::c_void,
					arg2: ::os::raw::c_int,
				) -> *mut ::os::raw::c_void,
			>,
			pub xSize: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xRoundup: ::option::Option<unsafe extern "C" fn(arg1: ::os::raw::c_int) -> ::os::raw::c_int,
			>,
			pub xInit: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xShutdown: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub pAppData: *mut ::os::raw::c_void,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_stmt
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct Mem
		{
			_unused: [u8; 0],
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_context
		{
			_unused: [u8; 0],
		}
		
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_module
		{
			pub iVersion: ::os::raw::c_int,
			pub xCreate: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3,
					pAux: *mut ::os::raw::c_void,
					argc: ::os::raw::c_int,
					argv: *const *const ::os::raw::c_char,
					ppVTab: *mut *mut sqlite3_vtab,
					arg2: *mut *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xConnect: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3,
					pAux: *mut ::os::raw::c_void,
					argc: ::os::raw::c_int,
					argv: *const *const ::os::raw::c_char,
					ppVTab: *mut *mut sqlite3_vtab,
					arg2: *mut *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xBestIndex: ::option::Option<unsafe extern "C" fn
                    (pVTab: *mut sqlite3_vtab,
					arg1: *mut sqlite3_index_info,
				) -> ::os::raw::c_int,
			>,
			pub xDisconnect: ::option::Option<unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xDestroy: ::option::Option<unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xOpen: ::option::Option<unsafe extern "C" fn
                    (pVTab: *mut sqlite3_vtab,
					ppCursor: *mut *mut sqlite3_vtab_cursor,
				) -> ::os::raw::c_int,
			>,
			pub xClose: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::os::raw::c_int,
			>,
			pub xFilter: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vtab_cursor,
					idxNum: ::os::raw::c_int,
					idxStr: *const ::os::raw::c_char,
					argc: ::os::raw::c_int,
					argv: *mut *mut sqlite3_value,
				) -> ::os::raw::c_int,
			>,
			pub xNext: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::os::raw::c_int,
			>,
			pub xEof: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::os::raw::c_int,
			>,
			pub xColumn: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vtab_cursor,
					arg2: *mut sqlite3_context,
					arg3: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRowid: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vtab_cursor,
					pRowid: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xUpdate: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_vtab,
					arg2: ::os::raw::c_int,
					arg3: *mut *mut sqlite3_value,
					arg4: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xBegin: ::option::Option<unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xSync: ::option::Option<unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xCommit: ::option::Option<unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xRollback: ::option::Option<unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xFindFunction: ::option::Option<unsafe extern "C" fn
                    (pVtab: *mut sqlite3_vtab,
					nArg: ::os::raw::c_int,
					zName: *const ::os::raw::c_char,
					pxFunc: *mut ::option::Option<
						unsafe extern "C" fn
                    (		arg1: *mut sqlite3_context,
							arg2: ::os::raw::c_int,
							arg3: *mut *mut sqlite3_value,
						),
					>,
					ppArg: *mut *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xRename: ::option::Option<unsafe extern "C" fn
                    (pVtab: *mut sqlite3_vtab,
					zNew: *const ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xSavepoint: ::option::Option<unsafe extern "C" fn
                    (pVTab: *mut sqlite3_vtab,
					arg1: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRelease: ::option::Option<unsafe extern "C" fn
                    (pVTab: *mut sqlite3_vtab,
					arg1: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRollbackTo: ::option::Option<unsafe extern "C" fn
                    (pVTab: *mut sqlite3_vtab,
					arg1: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_info
		{
			pub nConstraint: ::os::raw::c_int,
			pub aConstraint: *mut sqlite3_index_constraint,
			pub nOrderBy: ::os::raw::c_int,
			pub aOrderBy: *mut sqlite3_index_orderby,
			pub aConstraintUsage: *mut sqlite3_index_constraint_usage,
			pub idxNum: ::os::raw::c_int,
			pub idxStr: *mut ::os::raw::c_char,
			pub needToFreeIdxStr: ::os::raw::c_int,
			pub orderByConsumed: ::os::raw::c_int,
			pub estimatedCost: f64,
			pub estimatedRows: sqlite3_int64,
			pub idxFlags: ::os::raw::c_int,
			pub colUsed: sqlite3_uint64,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_constraint
		{
			pub iColumn: ::os::raw::c_int,
			pub op: ::os::raw::c_uchar,
			pub usable: ::os::raw::c_uchar,
			pub iTermOffset: ::os::raw::c_int,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_orderby
		{
			pub iColumn: ::os::raw::c_int,
			pub desc: ::os::raw::c_uchar,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_constraint_usage
		{
			pub argvIndex: ::os::raw::c_int,
			pub omit: ::os::raw::c_uchar,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_vtab
		{
			pub pModule: *const sqlite3_module,
			pub nRef: ::os::raw::c_int,
			pub zErrMsg: *mut ::os::raw::c_char,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_vtab_cursor
		{
			pub pVtab: *mut sqlite3_vtab,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_blob
		{
			_unused: [u8; 0],
		}
        
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_mutex_methods
		{
			pub xMutexInit: ::option::Option<unsafe extern "C" fn() -> ::os::raw::c_int>,
			pub xMutexEnd: ::option::Option<unsafe extern "C" fn() -> ::os::raw::c_int>,
			pub xMutexAlloc: ::option::Option<unsafe extern "C" fn(arg1: ::os::raw::c_int) -> *mut sqlite3_mutex,
			>,
			pub xMutexFree: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
			pub xMutexEnter: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
			pub xMutexTry: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int,
			>,
			pub xMutexLeave: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
			pub xMutexHeld: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int,
			>,
			pub xMutexNotheld: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int,
			>,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache_page
		{
			pub pBuf: *mut ::os::raw::c_void,
			pub pExtra: *mut ::os::raw::c_void,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache_methods2
		{
			pub iVersion: ::os::raw::c_int,
			pub pArg: *mut ::os::raw::c_void,
			pub xInit: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xShutdown: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub xCreate: ::option::Option<unsafe extern "C" fn
                    (szPage: ::os::raw::c_int,
					szExtra: ::os::raw::c_int,
					bPurgeable: ::os::raw::c_int,
				) -> *mut sqlite3_pcache,
			>,
			pub xCachesize: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache, nCachesize: ::os::raw::c_int),
			>,
			pub xPagecount: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache) -> ::os::raw::c_int,
			>,
			pub xFetch: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_pcache,
					key: ::os::raw::c_uint,
					createFlag: ::os::raw::c_int,
				) -> *mut sqlite3_pcache_page,
			>,
			pub xUnpin: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_pcache,
					arg2: *mut sqlite3_pcache_page,
					discard: ::os::raw::c_int,
				),
			>,
			pub xRekey: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_pcache,
					arg2: *mut sqlite3_pcache_page,
					oldKey: ::os::raw::c_uint,
					newKey: ::os::raw::c_uint,
				),
			>,
			pub xTruncate: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache, iLimit: ::os::raw::c_uint),
			>,
			pub xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
			pub xShrink: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache_methods
		{
			pub pArg: *mut ::os::raw::c_void,
			pub xInit: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xShutdown: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub xCreate: ::option::Option<unsafe extern "C" fn
                    (szPage: ::os::raw::c_int,
					bPurgeable: ::os::raw::c_int,
				) -> *mut sqlite3_pcache,
			>,
			pub xCachesize: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache, nCachesize: ::os::raw::c_int),
			>,
			pub xPagecount: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache) -> ::os::raw::c_int,
			>,
			pub xFetch: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_pcache,
					key: ::os::raw::c_uint,
					createFlag: ::os::raw::c_int,
				) -> *mut ::os::raw::c_void,
			>,
			pub xUnpin: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_pcache,
					arg2: *mut ::os::raw::c_void,
					discard: ::os::raw::c_int,
				),
			>,
			pub xRekey: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut sqlite3_pcache,
					arg2: *mut ::os::raw::c_void,
					oldKey: ::os::raw::c_uint,
					newKey: ::os::raw::c_uint,
				),
			>,
			pub xTruncate: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache, iLimit: ::os::raw::c_uint),
			>,
			pub xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_backup
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_snapshot
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_rtree_geometry
		{
			pub pContext: *mut ::os::raw::c_void,
			pub nParam: ::os::raw::c_int,
			pub aParam: *mut sqlite3_rtree_dbl,
			pub pUser: *mut ::os::raw::c_void,
			pub xDelUser: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_rtree_query_info
		{
			pub pContext: *mut ::os::raw::c_void,
			pub nParam: ::os::raw::c_int,
			pub aParam: *mut sqlite3_rtree_dbl,
			pub pUser: *mut ::os::raw::c_void,
			pub xDelUser: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub aCoord: *mut sqlite3_rtree_dbl,
			pub anQueue: *mut ::os::raw::c_uint,
			pub nCoord: ::os::raw::c_int,
			pub iLevel: ::os::raw::c_int,
			pub mxLevel: ::os::raw::c_int,
			pub iRowid: sqlite3_int64,
			pub rParentScore: sqlite3_rtree_dbl,
			pub eParentWithin: ::os::raw::c_int,
			pub eWithin: ::os::raw::c_int,
			pub rScore: sqlite3_rtree_dbl,
			pub apSqlParam: *mut *mut sqlite3_value,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_session
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_changeset_iter
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_changegroup
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct Fts5Context
		{
			_unused: [u8; 0],
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct Fts5PhraseIter
		{
			pub a: *const ::os::raw::c_uchar,
			pub b: *const ::os::raw::c_uchar,
		}
        
		#[repr( C )] #[derive(Debug, Copy, Clone)]
		pub struct Fts5ExtensionApi
		{
			pub iVersion: ::os::raw::c_int,
			pub xUserData: ::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Context) -> *mut ::os::raw::c_void,
			>,
			pub xColumnCount: ::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Context) -> ::os::raw::c_int,
			>,
			pub xRowCount: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					pnRow: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xColumnTotalSize: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iCol: ::os::raw::c_int,
					pnToken: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xTokenize: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					pText: *const ::os::raw::c_char,
					nText: ::os::raw::c_int,
					pCtx: *mut ::os::raw::c_void,
					xToken: ::option::Option<
						unsafe extern "C" fn
                    (		arg1: *mut ::os::raw::c_void,
							arg2: ::os::raw::c_int,
							arg3: *const ::os::raw::c_char,
							arg4: ::os::raw::c_int,
							arg5: ::os::raw::c_int,
							arg6: ::os::raw::c_int,
						) -> ::os::raw::c_int,
					>,
				) -> ::os::raw::c_int,
			>,
			pub xPhraseCount: ::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Context) -> ::os::raw::c_int,
			>,
			pub xPhraseSize: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xInstCount: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					pnInst: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xInst: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iIdx: ::os::raw::c_int,
					piPhrase: *mut ::os::raw::c_int,
					piCol: *mut ::os::raw::c_int,
					piOff: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRowid:
				::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Context) -> sqlite3_int64>,
			pub xColumnText: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iCol: ::os::raw::c_int,
					pz: *mut *const ::os::raw::c_char,
					pn: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xColumnSize: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iCol: ::os::raw::c_int,
					pnToken: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xQueryPhrase: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
					pUserData: *mut ::os::raw::c_void,
					arg2: ::option::Option<
						unsafe extern "C" fn
                    (		arg1: *const Fts5ExtensionApi,
							arg2: *mut Fts5Context,
							arg3: *mut ::os::raw::c_void,
						) -> ::os::raw::c_int,
					>,
				) -> ::os::raw::c_int,
			>,
			pub xSetAuxdata: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					pAux: *mut ::os::raw::c_void,
					xDelete: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				) -> ::os::raw::c_int,
			>,
			pub xGetAuxdata: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					bClear: ::os::raw::c_int,
				) -> *mut ::os::raw::c_void,
			>,
			pub xPhraseFirst: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
					arg2: *mut Fts5PhraseIter,
					arg3: *mut ::os::raw::c_int,
					arg4: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xPhraseNext: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					arg2: *mut Fts5PhraseIter,
					piCol: *mut ::os::raw::c_int,
					piOff: *mut ::os::raw::c_int,
				),
			>,
			pub xPhraseFirstColumn: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
					arg2: *mut Fts5PhraseIter,
					arg3: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xPhraseNextColumn: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Context,
					arg2: *mut Fts5PhraseIter,
					piCol: *mut ::os::raw::c_int,
				),
			>,
		}

		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct Fts5Tokenizer
		{
			_unused: [u8; 0],
		}

		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct fts5_tokenizer
		{
			pub xCreate: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut ::os::raw::c_void,
					azArg: *mut *const ::os::raw::c_char,
					nArg: ::os::raw::c_int,
					ppOut: *mut *mut Fts5Tokenizer,
				) -> ::os::raw::c_int,
			>,
			pub xDelete: ::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Tokenizer)>,
			pub xTokenize: ::option::Option<unsafe extern "C" fn
                    (arg1: *mut Fts5Tokenizer,
					pCtx: *mut ::os::raw::c_void,
					flags: ::os::raw::c_int,
					pText: *const ::os::raw::c_char,
					nText: ::os::raw::c_int,
					xToken: ::option::Option<
						unsafe extern "C" fn
                    (		pCtx: *mut ::os::raw::c_void,
							tflags: ::os::raw::c_int,
							pToken: *const ::os::raw::c_char,
							nToken: ::os::raw::c_int,
							iStart: ::os::raw::c_int,
							iEnd: ::os::raw::c_int,
						) -> ::os::raw::c_int,
					>,
				) -> ::os::raw::c_int,
			>,
		}

		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct fts5_api
		{
			pub iVersion: ::os::raw::c_int,
			pub xCreateTokenizer: ::option::Option<unsafe extern "C" fn
            (
                pApi: *mut fts5_api,
                zName: *const ::os::raw::c_char,
                pContext: *mut ::os::raw::c_void,
                pTokenizer: *mut fts5_tokenizer,
                xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
            ) -> ::os::raw::c_int>,
			pub xFindTokenizer: ::option::Option<unsafe extern "C" fn
            (
                pApi: *mut fts5_api,
                zName: *const ::os::raw::c_char,
                ppContext: *mut *mut ::os::raw::c_void,
                pTokenizer: *mut fts5_tokenizer,
            ) -> ::os::raw::c_int>,
			pub xCreateFunction: ::option::Option<unsafe extern "C" fn
            (
                pApi: *mut fts5_api,
                zName: *const ::os::raw::c_char,
                pContext: *mut ::os::raw::c_void,
                xFunction: fts5_extension_function,
                xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
            ) -> ::os::raw::c_int>,
		}

	} pub use self::bindings::*;

	impl Default for sqlite3_vtab 
	{
		fn default() -> Self { unsafe { mem::zeroed() } }
	}

	impl Default for sqlite3_vtab_cursor
	{
		fn default() -> Self { unsafe { mem::zeroed() } }
	}
	pub mod error
	{
		/*!
		*/
		use ::
		{
            ffi::{ c_char, c_int, NulError },
            sqlite3::
            {
                types::
                {
                    FromSqlError,
                    Type,
                },
                errmsg_to_string, Result
            },
			*,
		};
		/*
		*/		
		/// Extended result codes.
		const SQLITE_ERROR_MISSING_COLLSEQ: c_int = super::SQLITE_ERROR | (1 << 8);
		const SQLITE_ERROR_RETRY: c_int = super::SQLITE_ERROR | (2 << 8);
		const SQLITE_ERROR_SNAPSHOT: c_int = super::SQLITE_ERROR | (3 << 8);
		const SQLITE_IOERR_BEGIN_ATOMIC: c_int = super::SQLITE_IOERR | (29 << 8);
		const SQLITE_IOERR_COMMIT_ATOMIC: c_int = super::SQLITE_IOERR | (30 << 8);
		const SQLITE_IOERR_ROLLBACK_ATOMIC: c_int = super::SQLITE_IOERR | (31 << 8);
		const SQLITE_IOERR_DATA: c_int = super::SQLITE_IOERR | (32 << 8);
		const SQLITE_IOERR_CORRUPTFS: c_int = super::SQLITE_IOERR | (33 << 8);
		const SQLITE_IOERR_IN_PAGE: c_int = super::SQLITE_IOERR | (34 << 8);
		const SQLITE_LOCKED_VTAB: c_int = super::SQLITE_LOCKED | (2 << 8);
		const SQLITE_BUSY_TIMEOUT: c_int = super::SQLITE_BUSY | (3 << 8);
		const SQLITE_CANTOPEN_SYMLINK: c_int = super::SQLITE_CANTOPEN | (6 << 8);
		const SQLITE_CORRUPT_SEQUENCE: c_int = super::SQLITE_CORRUPT | (2 << 8);
		const SQLITE_CORRUPT_INDEX: c_int = super::SQLITE_CORRUPT | (3 << 8);
		const SQLITE_READONLY_CANTINIT: c_int = super::SQLITE_READONLY | (5 << 8);
		const SQLITE_READONLY_DIRECTORY: c_int = super::SQLITE_READONLY | (6 << 8);
		const SQLITE_CONSTRAINT_PINNED: c_int = super::SQLITE_CONSTRAINT | (11 << 8);
		const SQLITE_CONSTRAINT_DATATYPE: c_int = super::SQLITE_CONSTRAINT | (12 << 8);
		/// Error Codes
		#[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq, Eq)]
		pub enum ErrorCode
		{
			/// Internal logic error in SQLite
			InternalMalfunction,
			/// Access permission denied
			PermissionDenied,
			/// Callback routine requested an abort
			OperationAborted,
			/// The database file is locked
			DatabaseBusy,
			/// A table in the database is locked
			DatabaseLocked,
			/// A `malloc()` failed
			OutOfMemory,
			/// Attempt to write a readonly database
			ReadOnly,
			/// Operation terminated by `sqlite3_interrupt()`
			OperationInterrupted,
			/// Some kind of disk I/O error occurred
			SystemIoFailure,
			/// The database disk image is malformed
			DatabaseCorrupt,
			/// Unknown opcode in `sqlite3_file_control()`
			NotFound,
			/// Insertion failed because database is full
			DiskFull,
			/// Unable to open the database file
			CannotOpen,
			/// Database lock protocol error
			FileLockingProtocolFailed,
			/// The database schema changed
			SchemaChanged,
			/// String or BLOB exceeds size limit
			TooBig,
			/// Abort due to constraint violation
			ConstraintViolation,
			/// Data type mismatch
			TypeMismatch,
			/// Library used incorrectly
			ApiMisuse,
			/// Uses OS features not supported on host
			NoLargeFileSupport,
			/// Authorization denied
			AuthorizationForStatementDenied,
			/// 2nd parameter to `sqlite3_bind` out of range
			ParameterOutOfRange,
			/// File opened that is not a database file
			NotADatabase,
			/// SQL error or missing database
			Unknown,
		}

		#[derive(Clone, Copy, Debug, PartialEq, Eq)]
		pub struct Errors
		{
			pub code: ErrorCode,
			pub extended_code: c_int,
		}

		impl Errors
		{
			#[must_use] pub fn new(result_code: c_int) -> Self
			{
				let code = match result_code & 0xff
				{
					super::SQLITE_INTERNAL => ErrorCode::InternalMalfunction,
					super::SQLITE_PERM => ErrorCode::PermissionDenied,
					super::SQLITE_ABORT => ErrorCode::OperationAborted,
					super::SQLITE_BUSY => ErrorCode::DatabaseBusy,
					super::SQLITE_LOCKED => ErrorCode::DatabaseLocked,
					super::SQLITE_NOMEM => ErrorCode::OutOfMemory,
					super::SQLITE_READONLY => ErrorCode::ReadOnly,
					super::SQLITE_INTERRUPT => ErrorCode::OperationInterrupted,
					super::SQLITE_IOERR => ErrorCode::SystemIoFailure,
					super::SQLITE_CORRUPT => ErrorCode::DatabaseCorrupt,
					super::SQLITE_NOTFOUND => ErrorCode::NotFound,
					super::SQLITE_FULL => ErrorCode::DiskFull,
					super::SQLITE_CANTOPEN => ErrorCode::CannotOpen,
					super::SQLITE_PROTOCOL => ErrorCode::FileLockingProtocolFailed,
					super::SQLITE_SCHEMA => ErrorCode::SchemaChanged,
					super::SQLITE_TOOBIG => ErrorCode::TooBig,
					super::SQLITE_CONSTRAINT => ErrorCode::ConstraintViolation,
					super::SQLITE_MISMATCH => ErrorCode::TypeMismatch,
					super::SQLITE_MISUSE => ErrorCode::ApiMisuse,
					super::SQLITE_NOLFS => ErrorCode::NoLargeFileSupport,
					super::SQLITE_AUTH => ErrorCode::AuthorizationForStatementDenied,
					super::SQLITE_RANGE => ErrorCode::ParameterOutOfRange,
					super::SQLITE_NOTADB => ErrorCode::NotADatabase,
					_ => ErrorCode::Unknown,
				};

				Self
				{
					code,
					extended_code: result_code,
				}
			}
		}

		impl fmt::Display for Errors
		{
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
			{
				write!
				(
					f,
					"Error code {}: {}",
					self.extended_code,
					code_to_str(self.extended_code)
				)
			}
		}

		impl error::Error for Errors
		{
			fn description(&self) -> &str 
			{ code_to_str(self.extended_code) }
		}

		#[must_use] pub fn code_to_str(code: c_int) -> &'static str
		{
			match code
			{
				super::SQLITE_OK        => "Successful result",
				super::SQLITE_ERROR     => "SQL error or missing database",
				super::SQLITE_INTERNAL  => "Internal logic error in SQLite",
				super::SQLITE_PERM      => "Access permission denied",
				super::SQLITE_ABORT     => "Callback routine requested an abort",
				super::SQLITE_BUSY      => "The database file is locked",
				super::SQLITE_LOCKED    => "A table in the database is locked",
				super::SQLITE_NOMEM     => "A malloc() failed",
				super::SQLITE_READONLY  => "Attempt to write a readonly database",
				super::SQLITE_INTERRUPT => "Operation terminated by sqlite3_interrupt()",
				super::SQLITE_IOERR     => "Some kind of disk I/O error occurred",
				super::SQLITE_CORRUPT   => "The database disk image is malformed",
				super::SQLITE_NOTFOUND  => "Unknown opcode in sqlite3_file_control()",
				super::SQLITE_FULL      => "Insertion failed because database is full",
				super::SQLITE_CANTOPEN  => "Unable to open the database file",
				super::SQLITE_PROTOCOL  => "Database lock protocol error",
				super::SQLITE_EMPTY     => "Database is empty",
				super::SQLITE_SCHEMA    => "The database schema changed",
				super::SQLITE_TOOBIG    => "String or BLOB exceeds size limit",
				super::SQLITE_CONSTRAINT=> "Abort due to constraint violation",
				super::SQLITE_MISMATCH  => "Data type mismatch",
				super::SQLITE_MISUSE    => "Library used incorrectly",
				super::SQLITE_NOLFS     => "Uses OS features not supported on host",
				super::SQLITE_AUTH      => "Authorization denied",
				super::SQLITE_FORMAT    => "Auxiliary database format error",
				super::SQLITE_RANGE     => "2nd parameter to sqlite3_bind out of range",
				super::SQLITE_NOTADB    => "File opened that is not a database file",
				super::SQLITE_NOTICE    => "Notifications from sqlite3_log()",
				super::SQLITE_WARNING   => "Warnings from sqlite3_log()",
				super::SQLITE_ROW       => "sqlite3_step() has another row ready",
				super::SQLITE_DONE      => "sqlite3_step() has finished executing",

				SQLITE_ERROR_MISSING_COLLSEQ   => "SQLITE_ERROR_MISSING_COLLSEQ",
				SQLITE_ERROR_RETRY   => "SQLITE_ERROR_RETRY",
				SQLITE_ERROR_SNAPSHOT   => "SQLITE_ERROR_SNAPSHOT",

				super::SQLITE_IOERR_READ              => "Error reading from disk",
				super::SQLITE_IOERR_SHORT_READ        => "Unable to obtain number of requested bytes (file truncated?)",
				super::SQLITE_IOERR_WRITE             => "Error writing to disk",
				super::SQLITE_IOERR_FSYNC             => "Error flushing data to persistent storage (fsync)",
				super::SQLITE_IOERR_DIR_FSYNC         => "Error calling fsync on a directory",
				super::SQLITE_IOERR_TRUNCATE          => "Error attempting to truncate file",
				super::SQLITE_IOERR_FSTAT             => "Error invoking fstat to get file metadata",
				super::SQLITE_IOERR_UNLOCK            => "I/O error within xUnlock of a VFS object",
				super::SQLITE_IOERR_RDLOCK            => "I/O error within xLock of a VFS object (trying to obtain a read lock)",
				super::SQLITE_IOERR_DELETE            => "I/O error within xDelete of a VFS object",
				super::SQLITE_IOERR_BLOCKED           => "SQLITE_IOERR_BLOCKED", // no longer used
				super::SQLITE_IOERR_NOMEM             => "Out of memory in I/O layer",
				super::SQLITE_IOERR_ACCESS            => "I/O error within xAccess of a VFS object",
				super::SQLITE_IOERR_CHECKRESERVEDLOCK => "I/O error within then xCheckReservedLock method",
				super::SQLITE_IOERR_LOCK              => "I/O error in the advisory file locking layer",
				super::SQLITE_IOERR_CLOSE             => "I/O error within the xClose method",
				super::SQLITE_IOERR_DIR_CLOSE         => "SQLITE_IOERR_DIR_CLOSE", // no longer used
				super::SQLITE_IOERR_SHMOPEN           => "I/O error within the xShmMap method (trying to open a new shared-memory segment)",
				super::SQLITE_IOERR_SHMSIZE           => "I/O error within the xShmMap method (trying to resize an existing shared-memory segment)",
				super::SQLITE_IOERR_SHMLOCK           => "SQLITE_IOERR_SHMLOCK", // no longer used
				super::SQLITE_IOERR_SHMMAP            => "I/O error within the xShmMap method (trying to map a shared-memory segment into process address space)",
				super::SQLITE_IOERR_SEEK              => "I/O error within the xRead or xWrite (trying to seek within a file)",
				super::SQLITE_IOERR_DELETE_NOENT      => "File being deleted does not exist",
				super::SQLITE_IOERR_MMAP              => "I/O error while trying to map or unmap part of the database file into process address space",
				super::SQLITE_IOERR_GETTEMPPATH       => "VFS is unable to determine a suitable directory for temporary files",
				super::SQLITE_IOERR_CONVPATH          => "cygwin_conv_path() system call failed",
				super::SQLITE_IOERR_VNODE             => "SQLITE_IOERR_VNODE", // not documented?
				super::SQLITE_IOERR_AUTH              => "SQLITE_IOERR_AUTH",
				SQLITE_IOERR_BEGIN_ATOMIC      => "SQLITE_IOERR_BEGIN_ATOMIC",
				SQLITE_IOERR_COMMIT_ATOMIC     => "SQLITE_IOERR_COMMIT_ATOMIC",
				SQLITE_IOERR_ROLLBACK_ATOMIC   => "SQLITE_IOERR_ROLLBACK_ATOMIC",
				SQLITE_IOERR_DATA   => "SQLITE_IOERR_DATA",
				SQLITE_IOERR_CORRUPTFS   => "SQLITE_IOERR_CORRUPTFS",
				SQLITE_IOERR_IN_PAGE   => "SQLITE_IOERR_IN_PAGE",

				super::SQLITE_LOCKED_SHAREDCACHE      => "Locking conflict due to another connection with a shared cache",
				SQLITE_LOCKED_VTAB             => "SQLITE_LOCKED_VTAB",

				super::SQLITE_BUSY_RECOVERY           => "Another process is recovering a WAL mode database file",
				super::SQLITE_BUSY_SNAPSHOT           => "Cannot promote read transaction to write transaction because of writes by another connection",
				SQLITE_BUSY_TIMEOUT           => "SQLITE_BUSY_TIMEOUT",

				super::SQLITE_CANTOPEN_NOTEMPDIR      => "SQLITE_CANTOPEN_NOTEMPDIR", // no longer used
				super::SQLITE_CANTOPEN_ISDIR          => "Attempted to open directory as file",
				super::SQLITE_CANTOPEN_FULLPATH       => "Unable to convert filename into full pathname",
				super::SQLITE_CANTOPEN_CONVPATH       => "cygwin_conv_path() system call failed",
				SQLITE_CANTOPEN_SYMLINK       => "SQLITE_CANTOPEN_SYMLINK",

				super::SQLITE_CORRUPT_VTAB            => "Content in the virtual table is corrupt",
				SQLITE_CORRUPT_SEQUENCE        => "SQLITE_CORRUPT_SEQUENCE",
				SQLITE_CORRUPT_INDEX        => "SQLITE_CORRUPT_INDEX",

				super::SQLITE_READONLY_RECOVERY       => "WAL mode database file needs recovery (requires write access)",
				super::SQLITE_READONLY_CANTLOCK       => "Shared-memory file associated with WAL mode database is read-only",
				super::SQLITE_READONLY_ROLLBACK       => "Database has hot journal that must be rolled back (requires write access)",
				super::SQLITE_READONLY_DBMOVED        => "Database cannot be modified because database file has moved",
				SQLITE_READONLY_CANTINIT       => "SQLITE_READONLY_CANTINIT",
				SQLITE_READONLY_DIRECTORY      => "SQLITE_READONLY_DIRECTORY",

				super::SQLITE_ABORT_ROLLBACK          => "Transaction was rolled back",

				super::SQLITE_CONSTRAINT_CHECK        => "A CHECK constraint failed",
				super::SQLITE_CONSTRAINT_COMMITHOOK   => "Commit hook caused rollback",
				super::SQLITE_CONSTRAINT_FOREIGNKEY   => "Foreign key constraint failed",
				super::SQLITE_CONSTRAINT_FUNCTION     => "Error returned from extension function",
				super::SQLITE_CONSTRAINT_NOTNULL      => "A NOT NULL constraint failed",
				super::SQLITE_CONSTRAINT_PRIMARYKEY   => "A PRIMARY KEY constraint failed",
				super::SQLITE_CONSTRAINT_TRIGGER      => "A RAISE function within a trigger fired",
				super::SQLITE_CONSTRAINT_UNIQUE       => "A UNIQUE constraint failed",
				super::SQLITE_CONSTRAINT_VTAB         => "An application-defined virtual table error occurred",
				super::SQLITE_CONSTRAINT_ROWID        => "A non-unique rowid occurred",
				SQLITE_CONSTRAINT_PINNED        => "SQLITE_CONSTRAINT_PINNED",
				SQLITE_CONSTRAINT_DATATYPE        => "SQLITE_CONSTRAINT_DATATYPE",

				super::SQLITE_NOTICE_RECOVER_WAL      => "A WAL mode database file was recovered",
				super::SQLITE_NOTICE_RECOVER_ROLLBACK => "Hot journal was rolled back",

				super::SQLITE_WARNING_AUTOINDEX       => "Automatic indexing used - database might benefit from additional indexes",

				super::SQLITE_AUTH_USER               => "SQLITE_AUTH_USER", // not documented?

				_ => "Unknown error code",
			}
		}
	
		/// Enum listing possible errors from rusqlite.
		#[derive(Debug)]
		#[non_exhaustive]
		pub enum Error
		{
			/// An error from an underlying SQLite call.
			SqliteFailure(Errors, Option<String>),

			/// Error reported when attempting to open a connection when SQLite was
			/// configured to allow single-threaded use only.
			SqliteSingleThreadedMode,

			/// Error when the value of a particular column is requested, but it cannot
			/// be converted to the requested Rust type.
			FromSqlConversionFailure(usize, Type, Box<dyn error::Error + Send + Sync + 'static>),

			/// Error when SQLite gives us an integral value outside the range of the
			/// requested type (e.g., trying to get the value 1000 into a `u8`).
			/// The associated `usize` is the column index,
			/// and the associated `i64` is the value returned by SQLite.
			IntegralValueOutOfRange(usize, i64),

			/// Error converting a string to UTF-8.
			Utf8Error(str::Utf8Error),

			/// Error converting a string to a C-compatible string because it contained
			/// an embedded nul.
			NulError(NulError),

			/// Error when using SQL named parameters and passing a parameter name not
			/// present in the SQL.
			InvalidParameterName(String),

			/// Error converting a file path to a string.
			InvalidPath(PathBuf),

			/// Error returned when an [`execute`](crate::Connection::execute) call
			/// returns rows.
			ExecuteReturnedResults,

			/// Error when a query that was expected to return at least one row (e.g.,
			/// for [`query_row`](crate::Connection::query_row)) did not return any.
			QueryReturnedNoRows,

			/// Error when a query that was expected to return only one row (e.g.,
			/// for [`query_one`](crate::Connection::query_one)) did return more than one.
			QueryReturnedMoreThanOneRow,

			/// Error when the value of a particular column is requested, but the index
			/// is out of range for the statement.
			InvalidColumnIndex(usize),

			/// Error when the value of a named column is requested, but no column
			/// matches the name for the statement.
			InvalidColumnName(String),

			/// Error when the value of a particular column is requested, but the type
			/// of the result in that column cannot be converted to the requested
			/// Rust type.
			InvalidColumnType(usize, String, Type),

			/// Error when a query that was expected to insert one row did not insert
			/// any or insert many.
			StatementChangedRows(usize),			

			/// Error available for the implementors of the
			/// [`ToSql`](crate::types::ToSql) trait.
			ToSqlConversionFailure(Box<dyn error::Error + Send + Sync + 'static>),

			/// Error when the SQL is not a `SELECT`, is not read-only.
			InvalidQuery,

			/// An unwinding panic occurs in a UDF (user-defined function).
			UnwindingPanic,

			/// Error when the SQL contains multiple statements.
			MultipleStatement,
			/// Error when the number of bound parameters does not match the number of
			/// parameters in the query. The first `usize` is how many parameters were
			/// given, the 2nd is how many were expected.
			InvalidParameterCount(usize, usize),
			
			/// Error referencing a specific token in the input SQL
			SqlInputError {
				/// error code
				error: Errors,
				/// error message
				msg: String,
				/// SQL input
				sql: String,
				/// byte offset of the start of invalid token
				offset: c_int,
			},
			/// Error when the schema of a particular database is requested, but the index is out of range.
			InvalidDatabaseIndex(usize),
		}

		impl PartialEq for Error {
			fn eq(&self, other: &Self) -> bool {
				match (self, other) {
					(Self::SqliteFailure(e1, s1), Self::SqliteFailure(e2, s2)) => e1 == e2 && s1 == s2,
					(Self::SqliteSingleThreadedMode, Self::SqliteSingleThreadedMode) => true,
					(Self::IntegralValueOutOfRange(i1, n1), Self::IntegralValueOutOfRange(i2, n2)) => {
						i1 == i2 && n1 == n2
					}
					(Self::Utf8Error(e1), Self::Utf8Error(e2)) => e1 == e2,
					(Self::NulError(e1), Self::NulError(e2)) => e1 == e2,
					(Self::InvalidParameterName(n1), Self::InvalidParameterName(n2)) => n1 == n2,
					(Self::InvalidPath(p1), Self::InvalidPath(p2)) => p1 == p2,
					(Self::ExecuteReturnedResults, Self::ExecuteReturnedResults) => true,
					(Self::QueryReturnedNoRows, Self::QueryReturnedNoRows) => true,
					(Self::QueryReturnedMoreThanOneRow, Self::QueryReturnedMoreThanOneRow) => true,
					(Self::InvalidColumnIndex(i1), Self::InvalidColumnIndex(i2)) => i1 == i2,
					(Self::InvalidColumnName(n1), Self::InvalidColumnName(n2)) => n1 == n2,
					(Self::InvalidColumnType(i1, n1, t1), Self::InvalidColumnType(i2, n2, t2)) => {
						i1 == i2 && t1 == t2 && n1 == n2
					}
					(Self::StatementChangedRows(n1), Self::StatementChangedRows(n2)) => n1 == n2,
					(Self::InvalidQuery, Self::InvalidQuery) => true,
					(Self::UnwindingPanic, Self::UnwindingPanic) => true,
					(Self::InvalidParameterCount(i1, n1), Self::InvalidParameterCount(i2, n2)) => {
						i1 == i2 && n1 == n2
					}
					(
						Self::SqlInputError {
							error: e1,
							msg: m1,
							sql: s1,
							offset: o1,
						},
						Self::SqlInputError {
							error: e2,
							msg: m2,
							sql: s2,
							offset: o2,
						},
					) => e1 == e2 && m1 == m2 && s1 == s2 && o1 == o2,
					(Self::InvalidDatabaseIndex(i1), Self::InvalidDatabaseIndex(i2)) => i1 == i2,
					(..) => false,
				}
			}
		}

		impl From<str::Utf8Error> for Error {
			#[cold]
			fn from(err: str::Utf8Error) -> Self {
				Self::Utf8Error(err)
			}
		}

		impl From<NulError> for Error {
			#[cold]
			fn from(err: NulError) -> Self {
				Self::NulError(err)
			}
		}

		const UNKNOWN_COLUMN: usize = usize::MAX;
		/// The conversion isn't precise, but it's convenient to have it
		/// to allow use of `get_raw(…).as_…()?` in callbacks that take `Error`.
		impl From<FromSqlError> for Error {
			#[cold]
			fn from(err: FromSqlError) -> Self {
				// The error type requires index and type fields, but they aren't known in this
				// context.
				match err {
					FromSqlError::OutOfRange(val) => Self::IntegralValueOutOfRange(UNKNOWN_COLUMN, val),
					FromSqlError::InvalidBlobSize { .. } => {
						Self::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Blob, Box::new(err))
					}
					FromSqlError::Other(source) => {
						Self::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Null, source)
					}
					_ => Self::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Null, Box::new(err)),
				}
			}
		}
		
		impl fmt::Display for Error {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {	match *self {
					Self::SqliteFailure(ref err, None) => err.fmt(f),
					Self::SqliteFailure(_, Some(ref s)) => write!(f, "{s}"),
					Self::SqliteSingleThreadedMode => write!(
						f,
						"SQLite was compiled or configured for single-threaded use only"
					),
					Self::FromSqlConversionFailure(i, ref t, ref err) => {
						if i != UNKNOWN_COLUMN {
							write!(f, "Conversion error from type {t} at index: {i}, {err}")
						} else {
							err.fmt(f)
						}
					}
					Self::IntegralValueOutOfRange(col, val) => {
						if col != UNKNOWN_COLUMN {
							write!(f, "Integer {val} out of range at index {col}")
						} else {
							write!(f, "Integer {val} out of range")
						}
					}
					Self::Utf8Error(ref err) => err.fmt(f),
					Self::NulError(ref err) => err.fmt(f),
					Self::InvalidParameterName(ref name) => write!(f, "Invalid parameter name: {name}"),
					Self::InvalidPath(ref p) => write!(f, "Invalid path: {}", p.to_string_lossy()),
					Self::ExecuteReturnedResults => {
						write!(f, "Execute returned results - did you mean to call query?")
					}
					Self::QueryReturnedNoRows => write!(f, "Query returned no rows"),
					Self::QueryReturnedMoreThanOneRow => write!(f, "Query returned more than one row"),
					Self::InvalidColumnIndex(i) => write!(f, "Invalid column index: {i}"),
					Self::InvalidColumnName(ref name) => write!(f, "Invalid column name: {name}"),
					Self::InvalidColumnType(i, ref name, ref t) => {
						write!(f, "Invalid column type {t} at index: {i}, name: {name}")
					}
					Self::InvalidParameterCount(i1, n1) => write!(
						f,
						"Wrong number of parameters passed to query. Got {i1}, needed {n1}"
					),
					Self::StatementChangedRows(i) => write!(f, "Query changed {i} rows"),
					Self::ToSqlConversionFailure(ref err) => err.fmt(f),
					Self::InvalidQuery => write!(f, "Query is not read-only"),
					Self::UnwindingPanic => write!(f, "unwinding panic"),
					Self::MultipleStatement => write!(f, "Multiple statements provided"),
					Self::SqlInputError {
						ref msg,
						offset,
						ref sql,
						..
					} => write!(f, "{msg} in {sql} at offset {offset}"),
					Self::InvalidDatabaseIndex(i) => write!(f, "Invalid database index: {i}"),
				}
			}
		}

		impl error::Error for Error {
			fn source(&self) -> Option<&(dyn error::Error + 'static)> {
				match *self {
					Self::SqliteFailure(ref err, _) => Some(err),
					Self::Utf8Error(ref err) => Some(err),
					Self::NulError(ref err) => Some(err),

					Self::IntegralValueOutOfRange(..)
					| Self::SqliteSingleThreadedMode
					| Self::InvalidParameterName(_)
					| Self::ExecuteReturnedResults
					| Self::QueryReturnedNoRows
					| Self::QueryReturnedMoreThanOneRow
					| Self::InvalidColumnIndex(_)
					| Self::InvalidColumnName(_)
					| Self::InvalidColumnType(..)
					| Self::InvalidPath(_)
					| Self::InvalidParameterCount(..)
					| Self::StatementChangedRows(_)
					| Self::InvalidQuery
					| Self::MultipleStatement => None,
					

					Self::FromSqlConversionFailure(_, _, ref err)
					| Self::ToSqlConversionFailure(ref err) => Some(&**err),
					

					Self::UnwindingPanic => None,
					Self::SqlInputError { ref error, .. } => Some(error),
					Self::InvalidDatabaseIndex(_) => None,
				}
			}
		}

		impl Error {
			/// Returns the underlying SQLite error if this is [`Error::SqliteFailure`].
			#[inline]
			#[must_use]
			pub fn sqlite_error(&self) -> Option<&ffi::Error> {
				match self {
					Self::SqliteFailure(error, _) => Some(error),
					_ => None,
				}
			}
			/// Returns the underlying SQLite error code if this is
			/// [`Error::SqliteFailure`].
			#[inline]
			#[must_use]
			pub fn sqlite_error_code(&self) -> Option<ffi::ErrorCode> {
				self.sqlite_error().map(|error| error.code)
			}
		}

		// These are public but not re-exported by lib.rs, so only visible within crate.

		#[cold]
		pub fn error_from_sqlite_code(code: c_int, message: Option<String>) -> Error {
			Error::SqliteFailure(ffi::Error::new(code), message)
		}

		#[cold]
		pub unsafe fn error_from_handle(db: *mut ffi::sqlite3, code: c_int) -> Error {
			error_from_sqlite_code(code, error_msg(db, code))
		}

		unsafe fn error_msg(db: *mut ffi::sqlite3, code: c_int) -> Option<String> {
			if db.is_null() || ffi::sqlite3_errcode(db) != code {
				let err_str = ffi::sqlite3_errstr(code);
				if err_str.is_null() {
					None
				} else {
					Some(errmsg_to_string(err_str))
				}
			} else {
				Some(errmsg_to_string(ffi::sqlite3_errmsg(db)))
			}
		}

		pub unsafe fn decode_result_raw(db: *mut ffi::sqlite3, code: c_int) -> Result<()> {
			if code == ffi::SQLITE_OK {
				Ok(())
			} else {
				Err(error_from_handle(db, code))
			}
		}
		
		#[cold]
		pub unsafe fn error_with_offset(db: *mut ffi::sqlite3, code: c_int, sql: &str) -> Error {
			if db.is_null() {
				error_from_sqlite_code(code, None)
			} else {
				let error = ffi::Error::new(code);
				let msg = error_msg(db, code);
				if ffi::ErrorCode::Unknown == error.code {
					let offset = ffi::sqlite3_error_offset(db);
					if offset >= 0 {
						return Error::SqlInputError {
							error,
							msg: msg.unwrap_or("error".to_owned()),
							sql: sql.to_owned(),
							offset,
						};
					}
				}
				Error::SqliteFailure(error, msg)
			}
		}

		pub fn check(code: c_int) -> Result<()> {
			if code != ffi::SQLITE_OK {
				Err(error_from_sqlite_code(code, None))
			} else {
				Ok(())
			}
		}
		/// Transform Rust error to SQLite error (message and code).
		/// # Safety
		/// This function is unsafe because it uses raw pointer
		pub unsafe fn to_sqlite_error(e: &Error, err_msg: *mut *mut c_char) -> c_int {
			use crate::util::alloc;
			match e {
				Error::SqliteFailure(err, s) => {
					if let Some(s) = s {
						*err_msg = alloc(s);
					}
					err.extended_code
				}
				err => {
					*err_msg = alloc(&err.to_string());
					ffi::SQLITE_ERROR
				}
			}
		}

	} pub use self::error::*;
	
	pub mod bind
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use crate::{ffi, Error, Result, Statement};
		use std::ffi::CStr;
        */
		mod sealed
		{
			use ::ffi::CStr;
			/// This trait exists just to ensure that the only impls of `trait BindIndex`
			/// that are allowed are ones in this crate.
			pub trait Sealed {}
			impl Sealed for usize {}
			impl Sealed for &str {}
			impl Sealed for &CStr {}
		}

		/// A trait implemented by types that can index into parameters of a statement.
		///
		/// It is only implemented for `usize` and `&str` and `&CStr`.
		pub trait BindIndex: sealed::Sealed {
			/// Returns the index of the associated parameter, or `Error` if no such
			/// parameter exists.
			fn idx(&self, stmt: &Statement<'_>) -> Result<usize>;
		}

		impl BindIndex for usize {
			#[inline] fn idx(&self, _: &Statement<'_>) -> Result<usize> {
				// No validation
				Ok(*self)
			}
		}

		impl BindIndex for &'_ str {
			fn idx(&self, stmt: &Statement<'_>) -> Result<usize> {
				match stmt.parameter_index(self)? {
					Some(idx) => Ok(idx),
					None => Err(Error::InvalidParameterName(self.to_string())),
				}
			}
		}
		/// C-string literal to avoid alloc
		impl BindIndex for &CStr {
			fn idx(&self, stmt: &Statement<'_>) -> Result<usize>
            {
				let r = unsafe { ffi::sqlite3_bind_parameter_index(stmt.ptr(), self.as_ptr()) };
				match r {
					0 => Err(Error::InvalidParameterName(
						self.to_string_lossy().to_string(),
					)),
					i => Ok(i as usize),
				}
			}
		}
    } pub use self::bind::BindIndex;

	pub mod busy
    {
        /*!
        Busy handler (when the database is locked) */
        use ::
        {
            ffi::{ c_int, c_void },
            panic::{ catch_unwind },
            time::lib::{ Duration },
            sqlite3::
            {
                Connection, InnerConnection, Result,
            },
            *,
        };
        /*
        */
		impl Connection
		{
			/// Set a busy handler that sleeps for a specified amount of time when a table is locked.
			pub fn busy_timeout(&self, timeout: Duration) -> Result<()>
            {
				let ms: i32 = timeout
					.as_secs()
					.checked_mul(1000)
					.and_then(|t| t.checked_add(timeout.subsec_millis().into()))
					.and_then(|t| t.try_into().ok())
					.expect("too big");
				self.db.borrow_mut().busy_timeout(ms)
			}
			/// Register a callback to handle `SQLITE_BUSY` errors.
			pub fn busy_handler(&self, callback: Option<fn(i32) -> bool>) -> Result<()>
            {
				unsafe extern "C" fn busy_handler_callback(p_arg: *mut c_void, count: c_int) -> c_int {
					let handler_fn: fn(i32) -> bool = mem::transmute(p_arg);
					c_int::from(catch_unwind(|| handler_fn(count)).unwrap_or_default())
				}
				let c = self.db.borrow_mut();
				let r = match callback {
					Some(f) => unsafe {
						ffi::sqlite3_busy_handler(c.db(), Some(busy_handler_callback), f as *mut c_void)
					},
					None => unsafe { ffi::sqlite3_busy_handler(c.db(), None, ptr::null_mut()) },
				};
				c.decode_result(r)
			}
		}

		impl InnerConnection
        {
			#[inline] fn busy_timeout(&mut self, timeout: c_int) -> Result<()>
            {
				let r = unsafe { ffi::sqlite3_busy_timeout(self.db, timeout) };
				self.decode_result(r)
			}
		}
    }

	pub mod cache
    {
        /*!
        Prepared statements cache for faster execution. */
        use ::
        {
            *,
        };
        /*
        use crate::raw_statement::RawStatement;
		use crate::{Connection, PrepFlags, Result, Statement};
		use hashlink::LruCache;
		use std::cell::RefCell;
		use std::ops::{Deref, DerefMut};
		use std::sync::Arc;
		*/
        impl Connection
		{
			/// Prepare a SQL statement for execution, returning a previously prepared
			/// (but not currently in-use) statement if one is available.
			#[inline] pub fn prepare_cached(&self, sql: &str) -> Result<CachedStatement<'_>> {
				self.cache.get(self, sql)
			}
			/// Set the maximum number of cached prepared statements this connection
			/// will hold.
			#[inline] pub fn set_prepared_statement_cache_capacity(&self, capacity: usize) {
				self.cache.set_capacity(capacity);
			}
			/// Remove/finalize all prepared statements currently in the cache.
			#[inline] pub fn flush_prepared_statement_cache(&self) {
				self.cache.flush();
			}
		}

		/// Prepared statements LRU cache.
		#[derive(Debug)]
		pub struct StatementCache(RefCell<LruCache<Arc<str>, RawStatement>>);

		unsafe impl Send for StatementCache {}

		/// Cacheable statement.
		pub struct CachedStatement<'conn> {
			stmt: Option<Statement<'conn>>,
			cache: &'conn StatementCache,
		}

		impl<'conn> Deref for CachedStatement<'conn> {
			type Target = Statement<'conn>;

			#[inline] fn deref(&self) -> &Statement<'conn> {
				self.stmt.as_ref().unwrap()
			}
		}

		impl<'conn> DerefMut for CachedStatement<'conn> {
			#[inline] fn deref_mut(&mut self) -> &mut Statement<'conn> {
				self.stmt.as_mut().unwrap()
			}
		}

		impl Drop for CachedStatement<'_> {
			#[inline] fn drop(&mut self) {
				if let Some(stmt) = self.stmt.take() {
					self.cache.cache_stmt(unsafe { stmt.into_raw() });
				}
			}
		}

		impl CachedStatement<'_> {
			#[inline] fn new<'conn>(stmt: Statement<'conn>, cache: &'conn StatementCache) -> CachedStatement<'conn> {
				CachedStatement {
					stmt: Some(stmt),
					cache,
				}
			}
			/// Discard the statement, preventing it from being returned to its
			/// [`Connection`]'s collection of cached statements.
			#[inline] pub fn discard(mut self) {
				self.stmt = None;
			}
		}

		impl StatementCache {
			/// Create a statement cache.
			#[inline] pub fn with_capacity(capacity: usize) -> Self {
				Self(RefCell::new(LruCache::new(capacity)))
			}

			#[inline] fn set_capacity(&self, capacity: usize) {
				self.0.borrow_mut().set_capacity(capacity);
			}

			// Search the cache for a prepared-statement object that implements `sql`.
			// If no such prepared-statement can be found, allocate and prepare a new one.
			//
			// # Failure
			//
			// Will return `Err` if no cached statement can be found and the underlying
			// SQLite prepare call fails.
			fn get<'conn>(
				&'conn self,
				conn: &'conn Connection,
				sql: &str,
			) -> Result<CachedStatement<'conn>>
            {
				let trimmed = sql.trim();
				let mut cache = self.0.borrow_mut();
				let stmt = match cache.remove(trimmed) {
					Some(raw_stmt) => Ok(Statement::new(conn, raw_stmt)),
					None => conn.prepare_with_flags(trimmed, PrepFlags::SQLITE_PREPARE_PERSISTENT),
				};
				stmt.map(|mut stmt| {
					stmt.stmt.set_statement_cache_key(trimmed);
					CachedStatement::new(stmt, self)
				})
			}

			// Return a statement to the cache.
			fn cache_stmt(&self, mut stmt: RawStatement) {
				if stmt.is_null() {
					return;
				}
				let mut cache = self.0.borrow_mut();
				stmt.clear_bindings();
				if let Some(sql) = stmt.statement_cache_key() {
					cache.insert(sql, stmt);
				} else {
					debug_assert!(
						false,
						"bug in statement cache code, statement returned to cache that without key"
					);
				}
			}

			#[inline] fn flush(&self) {
				let mut cache = self.0.borrow_mut();
				cache.clear();
			}
		}
    } pub use self::cache::{ StatementCache, CachedStatement };

	pub mod column
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use std::ffi::{c_char, CStr};
		use std::ptr;
		use std::str;

		use crate::ffi;
		use crate::{Connection, Error, Name, Result, Statement};
		*/
		/// Information about a column of a SQLite query.
		#[cfg(feature = "column_decltype")]
		#[derive(Debug)]
		pub struct Column<'stmt> {
			name: &'stmt str,
			decl_type: Option<&'stmt str>,
		}

		#[cfg(feature = "column_decltype")]
		impl Column<'_> {
			/// Returns the name of the column.
			#[inline]
			#[must_use]
			pub fn name(&self) -> &str {
				self.name
			}
			/// Returns the type of the column (`None` for expression).
			#[inline]
			#[must_use]
			pub fn decl_type(&self) -> Option<&str> {
				self.decl_type
			}
		}

		/// Metadata about the origin of a column of a SQLite query
		#[cfg(feature = "column_metadata")]
		#[derive(Debug)]
		pub struct ColumnMetadata<'stmt> {
			name: &'stmt str,
			database_name: Option<&'stmt str>,
			table_name: Option<&'stmt str>,
			origin_name: Option<&'stmt str>,
		}

		#[cfg(feature = "column_metadata")]
		impl ColumnMetadata<'_> {
			#[inline]
			#[must_use]
			/// Returns the name of the column in the query results
			pub fn name(&self) -> &str {
				self.name
			}

			#[inline]
			#[must_use]
			/// Returns the database name from which the column originates
			pub fn database_name(&self) -> Option<&str> {
				self.database_name
			}

			#[inline]
			#[must_use]
			/// Returns the table name from which the column originates
			pub fn table_name(&self) -> Option<&str> {
				self.table_name
			}

			#[inline]
			#[must_use]
			/// Returns the column name from which the column originates
			pub fn origin_name(&self) -> Option<&str> {
				self.origin_name
			}
		}

		impl Statement<'_> {
			/// Get all the column names in the result set of the prepared statement.
			///
			/// If associated DB schema can be altered concurrently, you should make
			/// sure that current statement has already been stepped once before
			/// calling this method.
			pub fn column_names(&self) -> Vec<&str>
            {
				let n = self.column_count();
				let mut cols = Vec::with_capacity(n);
				for i in 0..n {
					let s = self.column_name_unwrap(i);
					cols.push(s);
				}
				cols
			}
			/// Return the number of columns in the result set returned by the prepared
			/// statement.
			///
			/// If associated DB schema can be altered concurrently, you should make
			/// sure that current statement has already been stepped once before
			/// calling this method.
			#[inline] pub fn column_count(&self) -> usize {
				self.stmt.column_count()
			}
			/// Check that column name reference lifetime is limited:
			/// <https://www.sqlite.org/c3ref/column_name.html>
			/// > The returned string pointer is valid...
			///
			/// `column_name` reference can become invalid if `stmt` is reprepared
			/// (because of schema change) when `query_row` is called. So we assert
			/// that a compilation error happens if this reference is kept alive:
			/// ```compile_fail
			/// use rusqlite::{Connection, Result};
			/// fn main() -> Result<()> {
			///     let db = Connection::open_in_memory()?;
			///     let mut stmt = db.prepare("SELECT 1 as x")?;
			///     let column_name = stmt.column_name(0)?;
			///     let x = stmt.query_row([], |r| r.get::<_, i64>(0))?; // E0502
			///     assert_eq!(1, x);
			///     assert_eq!("x", column_name);
			///     Ok(())
			/// }
			/// ```
			#[inline]
			pub(super) fn column_name_unwrap(&self, col: usize) -> &str {
				// Just panic if the bounds are wrong for now, we never call this
				// without checking first.
				self.column_name(col).expect("Column out of bounds")
			}
			/// Returns the name assigned to a particular column in the result set
			/// returned by the prepared statement.
			///
			/// If associated DB schema can be altered concurrently, you should make
			/// sure that current statement has already been stepped once before
			/// calling this method.
			///
			/// ## Failure
			///
			/// Returns an `Error::InvalidColumnIndex` if `idx` is outside the valid
			/// column range for this row.
			///
			/// # Panics
			///
			/// Panics when column name is not valid UTF-8.
			#[inline] pub fn column_name(&self, col: usize) -> Result<&str> {
				self.stmt
					.column_name(col)
					// clippy::or_fun_call (nightly) vs clippy::unnecessary-lazy-evaluations (stable)
					.ok_or(Error::InvalidColumnIndex(col))
					.map(|slice| {
						slice
							.to_str()
							.expect("Invalid UTF-8 sequence in column name")
					})
			}
			/// Returns the column index in the result set for a given column name.
			///
			/// If there is no AS clause then the name of the column is unspecified and
			/// may change from one release of SQLite to the next.
			///
			/// If associated DB schema can be altered concurrently, you should make
			/// sure that current statement has already been stepped once before
			/// calling this method.
			///
			/// # Failure
			///
			/// Will return an `Error::InvalidColumnName` when there is no column with
			/// the specified `name`.
			#[inline] pub fn column_index(&self, name: &str) -> Result<usize>
            {
				let bytes = name.as_bytes();
				let n = self.column_count();
				for i in 0..n {
					// Note: `column_name` is only fallible if `i` is out of bounds,
					// which we've already checked.
					if bytes.eq_ignore_ascii_case(self.stmt.column_name(i).unwrap().to_bytes()) {
						return Ok(i);
					}
				}
				Err(Error::InvalidColumnName(String::from(name)))
			}
			/// Returns a slice describing the columns of the result of the query.
			///
			/// If associated DB schema can be altered concurrently, you should make
			/// sure that current statement has already been stepped once before
			/// calling this method.
			#[cfg(feature = "column_decltype")]
			pub fn columns(&self) -> Vec<Column<'_>>
            {
				let n = self.column_count();
				let mut cols = Vec::with_capacity(n);
				for i in 0..n {
					let name = self.column_name_unwrap(i);
					let slice = self.stmt.column_decltype(i);
					let decl_type = slice.map(|s| {
						s.to_str()
							.expect("Invalid UTF-8 sequence in column declaration")
					});
					cols.push(Column { name, decl_type });
				}
				cols
			}
			/// Returns the names of the database, table, and row from which
			/// each column of this query's results originate.
			///
			/// Computed or otherwise derived columns will have None values for these fields.
			#[cfg(feature = "column_metadata")]
			pub fn columns_with_metadata(&self) -> Vec<ColumnMetadata<'_>>
            {
				let n = self.column_count();
				let mut col_mets = Vec::with_capacity(n);
				for i in 0..n {
					let name = self.column_name_unwrap(i);
					let db_slice = self.stmt.column_database_name(i);
					let tbl_slice = self.stmt.column_table_name(i);
					let origin_slice = self.stmt.column_origin_name(i);
					col_mets.push(ColumnMetadata {
						name,
						database_name: db_slice.map(|s| {
							s.to_str()
								.expect("Invalid UTF-8 sequence in column db name")
						}),
						table_name: tbl_slice.map(|s| {
							s.to_str()
								.expect("Invalid UTF-8 sequence in column table name")
						}),
						origin_name: origin_slice.map(|s| {
							s.to_str()
								.expect("Invalid UTF-8 sequence in column origin name")
						}),
					})
				}
				col_mets
			}
			/// Extract metadata of column at specified index
			///
			/// Returns:
			/// - database name
			/// - table name
			/// - original column name
			/// - declared data type
			/// - name of default collation sequence
			/// - True if column has a NOT NULL constraint
			/// - True if column is part of the PRIMARY KEY
			/// - True if column is AUTOINCREMENT
			///
			/// See [Connection::column_metadata]
			#[cfg(feature = "column_metadata")]
			#[allow(clippy::type_complexity)]
			pub fn column_metadata(
				&self,
				col: usize,
			) -> Result<
				Option<(
					&CStr,
					&CStr,
					&CStr,
					Option<&CStr>,
					Option<&CStr>,
					bool,
					bool,
					bool,
				)>,
			>
            {
				let db_name = self.stmt.column_database_name(col);
				let table_name = self.stmt.column_table_name(col);
				let origin_name = self.stmt.column_origin_name(col);
				if db_name.is_none() || table_name.is_none() || origin_name.is_none() {
					return Ok(None);
				}
				let (data_type, coll_seq, not_null, primary_key, auto_inc) =
					self.conn
						.column_metadata(db_name, table_name.unwrap(), origin_name.unwrap())?;
				Ok(Some((
					db_name.unwrap(),
					table_name.unwrap(),
					origin_name.unwrap(),
					data_type,
					coll_seq,
					not_null,
					primary_key,
					auto_inc,
				)))
			}
		}

		impl Connection {
			/// Check if `table_name`.`column_name` exists.
			///
			/// `db_name` is main, temp, the name in ATTACH, or `None` to search all databases.
			pub fn column_exists<N: Name>(
				&self,
				db_name: Option<N>,
				table_name: N,
				column_name: N,
			) -> Result<bool> {
				self.exists(db_name, table_name, Some(column_name))
			}
			/// Check if `table_name` exists.
			///
			/// `db_name` is main, temp, the name in ATTACH, or `None` to search all databases.
			pub fn table_exists<N: Name>(&self, db_name: Option<N>, table_name: N) -> Result<bool> {
				self.exists(db_name, table_name, None)
			}
			/// Extract metadata of column at specified index
			///
			/// Returns:
			/// - declared data type
			/// - name of default collation sequence
			/// - True if column has a NOT NULL constraint
			/// - True if column is part of the PRIMARY KEY
			/// - True if column is AUTOINCREMENT
			#[allow(clippy::type_complexity)]
			pub fn column_metadata<N: Name>(
				&self,
				db_name: Option<N>,
				table_name: N,
				column_name: N,
			) -> Result<(Option<&CStr>, Option<&CStr>, bool, bool, bool)>
            {
				let cs = db_name.as_ref().map(N::as_cstr).transpose()?;
				let db_name = cs.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null());
				let table_name = table_name.as_cstr()?;
				let column_name = column_name.as_cstr()?;

				let mut data_type: *const c_char = ptr::null_mut();
				let mut coll_seq: *const c_char = ptr::null_mut();
				let mut not_null = 0;
				let mut primary_key = 0;
				let mut auto_inc = 0;

				self.decode_result(unsafe {
					ffi::sqlite3_table_column_metadata(
						self.handle(),
						db_name,
						table_name.as_ptr(),
						column_name.as_ptr(),
						&mut data_type,
						&mut coll_seq,
						&mut not_null,
						&mut primary_key,
						&mut auto_inc,
					)
				})?;

				Ok((
					if data_type.is_null() {
						None
					} else {
						Some(unsafe { CStr::from_ptr(data_type) })
					},
					if coll_seq.is_null() {
						None
					} else {
						Some(unsafe { CStr::from_ptr(coll_seq) })
					},
					not_null != 0,
					primary_key != 0,
					auto_inc != 0,
				))
			}

			fn exists<N: Name>(
				&self,
				db_name: Option<N>,
				table_name: N,
				column_name: Option<N>,
			) -> Result<bool>
            {
				let cs = db_name.as_ref().map(N::as_cstr).transpose()?;
				let db_name = cs.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null());
				let table_name = table_name.as_cstr()?;
				let cn = column_name.as_ref().map(N::as_cstr).transpose()?;
				let column_name = cn.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null());
				let r = unsafe {
					ffi::sqlite3_table_column_metadata(
						self.handle(),
						db_name,
						table_name.as_ptr(),
						column_name,
						ptr::null_mut(),
						ptr::null_mut(),
						ptr::null_mut(),
						ptr::null_mut(),
						ptr::null_mut(),
					)
				};
				match r {
					ffi::SQLITE_OK => Ok(true),
					ffi::SQLITE_ERROR => Ok(false),
					_ => self.db.borrow().decode_result(r).map(|_| false),
				}
			}
		}
        
    }

	pub mod config
    {
        /*!
        Configure database connections */
        use ::
        {
            *,
        };
        /*
        use std::ffi::c_int;
		use crate::error::check;
		use crate::ffi;
		use crate::{Connection, Result};
		*/
        /// Database Connection Configuration Options
		/// See [Database Connection Configuration Options](https://sqlite.org/c3ref/c_dbconfig_enable_fkey.html) for details.
		#[repr(i32)]
		#[derive(Copy, Clone, Debug)]
		#[expect(non_camel_case_types)]
		#[non_exhaustive]
		pub enum DbConfig
		{
			//SQLITE_DBCONFIG_MAINDBNAME = 1000, /* const char* */
			//SQLITE_DBCONFIG_LOOKASIDE = 1001,  /* void* int int */
			/// Enable or disable the enforcement of foreign key constraints.
			SQLITE_DBCONFIG_ENABLE_FKEY = ffi::SQLITE_DBCONFIG_ENABLE_FKEY,
			/// Enable or disable triggers.
			SQLITE_DBCONFIG_ENABLE_TRIGGER = ffi::SQLITE_DBCONFIG_ENABLE_TRIGGER,
			/// Enable or disable the `fts3_tokenizer()` function which is part of the
			/// FTS3 full-text search engine extension.
			SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = ffi::SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER, // 3.12.0
			//SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005,
			/// In WAL mode, enable or disable the checkpoint operation before closing
			/// the connection.
			SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE = 1006, // 3.16.2
			/// Activates or deactivates the query planner stability guarantee (QPSG).
			SQLITE_DBCONFIG_ENABLE_QPSG = 1007, // 3.20.0
			/// Includes or excludes output for any operations performed by trigger
			/// programs from the output of EXPLAIN QUERY PLAN commands.
			SQLITE_DBCONFIG_TRIGGER_EQP = 1008, // 3.22.0
			/// Activates or deactivates the "reset" flag for a database connection.
			/// Run VACUUM with this flag set to reset the database.
			SQLITE_DBCONFIG_RESET_DATABASE = 1009, // 3.24.0
			/// Activates or deactivates the "defensive" flag for a database connection.
			SQLITE_DBCONFIG_DEFENSIVE = 1010, // 3.26.0
			/// Activates or deactivates the `writable_schema` flag.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_WRITABLE_SCHEMA = 1011, // 3.28.0
			/// Activates or deactivates the legacy behavior of the ALTER TABLE RENAME
			/// command.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_LEGACY_ALTER_TABLE = 1012, // 3.29
			/// Activates or deactivates the legacy double-quoted string literal
			/// misfeature for DML statements only.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_DQS_DML = 1013, // 3.29.0
			/// Activates or deactivates the legacy double-quoted string literal
			/// misfeature for DDL statements.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_DQS_DDL = 1014, // 3.29.0
			/// Enable or disable views.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_ENABLE_VIEW = 1015, // 3.30.0
			/// Activates or deactivates the legacy file format flag.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_LEGACY_FILE_FORMAT = 1016, // 3.31.0
			/// Tells SQLite to assume that database schemas (the contents of the
			/// `sqlite_master` tables) are untainted by malicious content.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_TRUSTED_SCHEMA = 1017, // 3.31.0
			/// Sets or clears a flag that enables collection of the
			/// `sqlite3_stmt_scanstatus_v2()` statistics
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_STMT_SCANSTATUS = 1018, // 3.42.0
			/// Changes the default order in which tables and indexes are scanned
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_REVERSE_SCANORDER = 1019, // 3.42.0
			/// Enables or disables the ability of the ATTACH DATABASE SQL command
			/// to create a new database file if the database filed named in the ATTACH command does not already exist.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_ENABLE_ATTACH_CREATE = 1020, // 3.49.0
			/// Enables or disables the ability of the ATTACH DATABASE SQL command to open a database for writing.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_ENABLE_ATTACH_WRITE = 1021, // 3.49.0
			/// Enables or disables the ability to include comments in SQL text.
			#[cfg(feature = "modern_sqlite")]
			SQLITE_DBCONFIG_ENABLE_COMMENTS = 1022, // 3.49.0
		}

		impl Connection {
			/// Returns the current value of a `config`.
			///
			/// - `SQLITE_DBCONFIG_ENABLE_FKEY`: return `false` or `true` to indicate
			///   whether FK enforcement is off or on
			/// - `SQLITE_DBCONFIG_ENABLE_TRIGGER`: return `false` or `true` to indicate
			///   whether triggers are disabled or enabled
			/// - `SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER`: return `false` or `true` to
			///   indicate whether `fts3_tokenizer` are disabled or enabled
			/// - `SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE`: return `false` to indicate
			///   checkpoints-on-close are not disabled or `true` if they are
			/// - `SQLITE_DBCONFIG_ENABLE_QPSG`: return `false` or `true` to indicate
			///   whether the QPSG is disabled or enabled
			/// - `SQLITE_DBCONFIG_TRIGGER_EQP`: return `false` to indicate
			///   output-for-trigger are not disabled or `true` if it is
			#[inline] pub fn db_config(&self, config: DbConfig) -> Result<bool>
            {
				let c = self.db.borrow();
				unsafe {
					let mut val = 0;
					check(ffi::sqlite3_db_config(
						c.db(),
						config as c_int,
						-1,
						&mut val,
					))?;
					Ok(val != 0)
				}
			}
			/// Make configuration changes to a database connection
			///
			/// - `SQLITE_DBCONFIG_ENABLE_FKEY`: `false` to disable FK enforcement,
			///   `true` to enable FK enforcement
			/// - `SQLITE_DBCONFIG_ENABLE_TRIGGER`: `false` to disable triggers, `true`
			///   to enable triggers
			/// - `SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER`: `false` to disable
			///   `fts3_tokenizer()`, `true` to enable `fts3_tokenizer()`
			/// - `SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE`: `false` (the default) to enable
			///   checkpoints-on-close, `true` to disable them
			/// - `SQLITE_DBCONFIG_ENABLE_QPSG`: `false` to disable the QPSG, `true` to
			///   enable QPSG
			/// - `SQLITE_DBCONFIG_TRIGGER_EQP`: `false` to disable output for trigger
			///   programs, `true` to enable it
			#[inline] pub fn set_db_config(&self, config: DbConfig, new_val: bool) -> Result<bool>
            {
				let c = self.db.borrow_mut();
				unsafe {
					let mut val = 0;
					check(ffi::sqlite3_db_config(
						c.db(),
						config as c_int,
						new_val as c_int,
						&mut val,
					))?;
					Ok(val != 0)
				}
			}
		}
    }

	pub mod inner_connection
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use std::ffi::{c_char, c_int, CStr};
		#[cfg(feature = "load_extension")]
		use std::path::Path;
		use std::ptr;
		use std::str;
		use std::sync::{Arc, Mutex};

		use super::ffi;
		use super::str_for_sqlite;
		use super::{Connection, InterruptHandle, Name, OpenFlags, PrepFlags, Result};
		use crate::error::{decode_result_raw, error_from_handle, error_with_offset, Error};
		use crate::raw_statement::RawStatement;
		use crate::statement::Statement;
		use crate::version_number;
        */
        pub struct InnerConnection
		{
			pub db: *mut ffi::sqlite3,
			// It's unsafe to call `sqlite3_close` while another thread is performing
			// a `sqlite3_interrupt`, and vice versa, so we take this mutex during
			// those functions. This protects a copy of the `db` pointer (which is
			// cleared on closing), however the main copy, `db`, is unprotected.
			// Otherwise, a long-running query would prevent calling interrupt, as
			// interrupt would only acquire the lock after the query's completion.
			interrupt_lock: Arc<Mutex<*mut ffi::sqlite3>>,
			#[cfg(feature = "hooks")]
			pub commit_hook: Option<Box<dyn FnMut() -> bool + Send>>,
			#[cfg(feature = "hooks")]
			pub rollback_hook: Option<Box<dyn FnMut() + Send>>,
			#[cfg(feature = "hooks")]
			#[expect(clippy::type_complexity)]
			pub update_hook: Option<Box<dyn FnMut(crate::hooks::Action, &str, &str, i64) + Send>>,
			#[cfg(feature = "hooks")]
			pub progress_handler: Option<Box<dyn FnMut() -> bool + Send>>,
			#[cfg(feature = "hooks")]
			pub authorizer: Option<crate::hooks::BoxedAuthorizer>,
			#[cfg(feature = "preupdate_hook")]
			#[expect(clippy::type_complexity)]
			pub preupdate_hook: Option<
				Box<dyn FnMut(crate::hooks::Action, &str, &str, &crate::hooks::PreUpdateCase) + Send>,
			>,
			owned: bool,
		}

		unsafe impl Send for InnerConnection {}

		impl InnerConnection {
			#[expect(clippy::mutex_atomic, clippy::arc_with_non_send_sync)] // See unsafe impl Send / Sync for InterruptHandle
			#[inline] pub unsafe fn new(db: *mut ffi::sqlite3, owned: bool) -> Self {
				Self {
					db,
					interrupt_lock: Arc::new(Mutex::new(if owned { db } else { ptr::null_mut() })),
					#[cfg(feature = "hooks")]
					commit_hook: None,
					#[cfg(feature = "hooks")]
					rollback_hook: None,
					#[cfg(feature = "hooks")]
					update_hook: None,
					#[cfg(feature = "hooks")]
					progress_handler: None,
					#[cfg(feature = "hooks")]
					authorizer: None,
					#[cfg(feature = "preupdate_hook")]
					preupdate_hook: None,
					owned,
				}
			}

			pub fn open_with_flags(
				c_path: &CStr,
				mut flags: OpenFlags,
				vfs: Option<&CStr>,
			) -> Result<Self> {
				ensure_safe_sqlite_threading_mode()?;

				let z_vfs = match vfs {
					Some(c_vfs) => c_vfs.as_ptr(),
					None => ptr::null(),
				};

				// turn on extended results code before opening database to have a better diagnostic if a failure happens
				let exrescode = if version_number() >= 3_037_000 {
					flags |= OpenFlags::SQLITE_OPEN_EXRESCODE;
					true
				} else {
					false // flag SQLITE_OPEN_EXRESCODE is ignored by SQLite version < 3.37.0
				};

				unsafe {
					let mut db: *mut ffi::sqlite3 = ptr::null_mut();
					let r = ffi::sqlite3_open_v2(c_path.as_ptr(), &mut db, flags.bits(), z_vfs);
					if r != ffi::SQLITE_OK {
						let e = if db.is_null() {
							err!(r, "{}", c_path.to_string_lossy())
						} else {
							let mut e = error_from_handle(db, r);
							if let Error::SqliteFailure(
								ffi::Error {
									code: ffi::ErrorCode::CannotOpen,
									..
								},
								Some(msg),
							) = e
							{
								e = err!(r, "{msg}: {}", c_path.to_string_lossy());
							}
							ffi::sqlite3_close(db);
							e
						};

						return Err(e);
					}

					// attempt to turn on extended results code; don't fail if we can't.
					if !exrescode {
						ffi::sqlite3_extended_result_codes(db, 1);
					}

					let r = ffi::sqlite3_busy_timeout(db, 5000);
					if r != ffi::SQLITE_OK {
						let e = error_from_handle(db, r);
						ffi::sqlite3_close(db);
						return Err(e);
					}

					Ok(Self::new(db, true))
				}
			}

			#[inline] pub fn db(&self) -> *mut ffi::sqlite3 {
				self.db
			}

			#[inline] pub fn decode_result(&self, code: c_int) -> Result<()>
            {
				unsafe { decode_result_raw(self.db(), code) }
			}

			pub fn close(&mut self) -> Result<()> {
				if self.db.is_null() {
					return Ok(());
				}
				self.remove_hooks();
				self.remove_preupdate_hook();
				let mut shared_handle = self.interrupt_lock.lock().unwrap();
				assert!(
					!self.owned || !shared_handle.is_null(),
					"Bug: Somehow interrupt_lock was cleared before the DB was closed"
				);
				if !self.owned {
					self.db = ptr::null_mut();
					return Ok(());
				}
				unsafe {
					let r = ffi::sqlite3_close(self.db);
					// Need to use _raw because _guard has a reference out, and
					// decode_result takes &mut self.
					let r = decode_result_raw(self.db, r);
					if r.is_ok() {
						*shared_handle = ptr::null_mut();
						self.db = ptr::null_mut();
					}
					r
				}
			}

			#[inline] pub fn get_interrupt_handle(&self) -> InterruptHandle {
				InterruptHandle {
					db_lock: Arc::clone(&self.interrupt_lock),
				}
			}

			#[inline]
			#[cfg(feature = "load_extension")]
			pub unsafe fn enable_load_extension(&mut self, onoff: c_int) -> Result<()>
            {
				let r = ffi::sqlite3_enable_load_extension(self.db, onoff);
				self.decode_result(r)
			}

			#[cfg(feature = "load_extension")]
			pub unsafe fn load_extension<N: Name>(
				&self,
				dylib_path: &Path,
				entry_point: Option<N>,
			) -> Result<()>
            {
				let dylib_str = super::path_to_cstring(dylib_path)?;
				let mut errmsg: *mut c_char = ptr::null_mut();
				let cs = entry_point.as_ref().map(N::as_cstr).transpose()?;
				let c_entry = cs.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null());
				let r = ffi::sqlite3_load_extension(self.db, dylib_str.as_ptr(), c_entry, &mut errmsg);
				if r == ffi::SQLITE_OK {
					Ok(())
				} else {
					let message = super::errmsg_to_string(errmsg);
					ffi::sqlite3_free(errmsg.cast::<std::ffi::c_void>());
					Err(crate::error::error_from_sqlite_code(r, Some(message)))
				}
			}

			#[inline] pub fn last_insert_rowid(&self) -> i64 {
				unsafe { ffi::sqlite3_last_insert_rowid(self.db()) }
			}

			pub fn prepare<'a>(
				&mut self,
				conn: &'a Connection,
				sql: &str,
				flags: PrepFlags,
			) -> Result<(Statement<'a>, usize)>
            {
				let mut c_stmt: *mut ffi::sqlite3_stmt = ptr::null_mut();
				let (c_sql, len, _) = str_for_sqlite(sql.as_bytes())?;
				let mut c_tail: *const c_char = ptr::null();
				#[cfg(not(feature = "unlock_notify"))]
				let r = unsafe { self.prepare_(c_sql, len, flags, &mut c_stmt, &mut c_tail) };
				#[cfg(feature = "unlock_notify")]
				let r = unsafe {
					use crate::unlock_notify;
					let mut rc;
					loop {
						rc = self.prepare_(c_sql, len, flags, &mut c_stmt, &mut c_tail);
						if !unlock_notify::is_locked(self.db, rc) {
							break;
						}
						rc = unlock_notify::wait_for_unlock_notify(self.db);
						if rc != ffi::SQLITE_OK {
							break;
						}
					}
					rc
				};
				// If there is an error, *ppStmt is set to NULL.
				if r != ffi::SQLITE_OK {
					return Err(unsafe { error_with_offset(self.db, r, sql) });
				}
				// If the input text contains no SQL (if the input is an empty string or a
				// comment) then *ppStmt is set to NULL.
				let tail = if c_tail.is_null() {
					0
				} else {
					let n = (c_tail as isize) - (c_sql as isize);
					if n <= 0 || n >= len as isize {
						0
					} else {
						n as usize
					}
				};
				Ok((
					Statement::new(conn, unsafe { RawStatement::new(c_stmt) }),
					tail,
				))
			}

			#[inline]
			#[cfg(not(feature = "modern_sqlite"))]
			unsafe fn prepare_(
				&self,
				z_sql: *const c_char,
				n_byte: c_int,
				_: PrepFlags,
				pp_stmt: *mut *mut ffi::sqlite3_stmt,
				pz_tail: *mut *const c_char,
			) -> c_int {
				ffi::sqlite3_prepare_v2(self.db(), z_sql, n_byte, pp_stmt, pz_tail)
			}

			#[inline]
			#[cfg(feature = "modern_sqlite")]
			unsafe fn prepare_(
				&self,
				z_sql: *const c_char,
				n_byte: c_int,
				flags: PrepFlags,
				pp_stmt: *mut *mut ffi::sqlite3_stmt,
				pz_tail: *mut *const c_char,
			) -> c_int {
				ffi::sqlite3_prepare_v3(self.db(), z_sql, n_byte, flags.bits(), pp_stmt, pz_tail)
			}

			#[inline] pub fn changes(&self) -> u64 {
				#[cfg(not(feature = "modern_sqlite"))]
				unsafe {
					ffi::sqlite3_changes(self.db()) as u64
				}
				#[cfg(feature = "modern_sqlite")] // 3.37.0
				unsafe {
					ffi::sqlite3_changes64(self.db()) as u64
				}
			}

			#[inline] pub fn total_changes(&self) -> u64 {
				#[cfg(not(feature = "modern_sqlite"))]
				unsafe {
					ffi::sqlite3_total_changes(self.db()) as u64
				}
				#[cfg(feature = "modern_sqlite")] // 3.37.0
				unsafe {
					ffi::sqlite3_total_changes64(self.db()) as u64
				}
			}

			#[inline] pub fn is_autocommit(&self) -> bool {
				unsafe { get_autocommit(self.db()) }
			}

			pub fn is_busy(&self) -> bool {
				let db = self.db();
				unsafe {
					let mut stmt = ffi::sqlite3_next_stmt(db, ptr::null_mut());
					while !stmt.is_null() {
						if ffi::sqlite3_stmt_busy(stmt) != 0 {
							return true;
						}
						stmt = ffi::sqlite3_next_stmt(db, stmt);
					}
				}
				false
			}

			pub fn cache_flush(&mut self) -> Result<()> {
				crate::error::check(unsafe { ffi::sqlite3_db_cacheflush(self.db()) })
			}

			#[cfg(not(feature = "hooks"))]
			#[inline] fn remove_hooks(&mut self) {}

			#[cfg(not(feature = "preupdate_hook"))]
			#[inline] fn remove_preupdate_hook(&mut self) {}

			pub fn db_readonly<N: Name>(&self, db_name: N) -> Result<bool>
            {
				let name = db_name.as_cstr()?;
				let r = unsafe { ffi::sqlite3_db_readonly(self.db, name.as_ptr()) };
				match r {
					0 => Ok(false),
					1 => Ok(true),
					-1 => Err(err!(
						ffi::SQLITE_MISUSE,
						"{db_name:?} is not the name of a database"
					)),
					_ => Err(err!(r, "Unexpected result")),
				}
			}

			#[cfg(feature = "modern_sqlite")] // 3.37.0
			pub fn txn_state<N: Name>(
				&self,
				db_name: Option<N>,
			) -> Result<super::transaction::TransactionState>
            {
				let cs = db_name.as_ref().map(N::as_cstr).transpose()?;
				let name = cs.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null());
				let r = unsafe { ffi::sqlite3_txn_state(self.db, name) };
				match r {
					0 => Ok(super::transaction::TransactionState::None),
					1 => Ok(super::transaction::TransactionState::Read),
					2 => Ok(super::transaction::TransactionState::Write),
					-1 => Err(err!(
						ffi::SQLITE_MISUSE,
						"{db_name:?} is not the name of a valid schema"
					)),
					_ => Err(err!(r, "Unexpected result")),
				}
			}

			#[inline] pub fn release_memory(&self) -> Result<()> {
				self.decode_result(unsafe { ffi::sqlite3_db_release_memory(self.db) })
			}

			#[cfg(feature = "modern_sqlite")] // 3.41.0
			pub fn is_interrupted(&self) -> bool {
				unsafe { ffi::sqlite3_is_interrupted(self.db) == 1 }
			}
		}

		#[inline]
		pub(crate) unsafe fn get_autocommit(ptr: *mut ffi::sqlite3) -> bool {
			ffi::sqlite3_get_autocommit(ptr) != 0
		}

		#[inline]
		pub(crate) unsafe fn db_filename<N: Name>(
			_: std::marker::PhantomData<&()>,
			ptr: *mut ffi::sqlite3,
			db_name: N,
		) -> Option<&str> {
			let db_name = db_name.as_cstr().unwrap();
			let db_filename = ffi::sqlite3_db_filename(ptr, db_name.as_ptr());
			if db_filename.is_null() {
				None
			} else {
				CStr::from_ptr(db_filename).to_str().ok()
			}
		}

		impl Drop for InnerConnection {
			#[expect(unused_must_use)]
			#[inline] fn drop(&mut self) {
				self.close();
			}
		}

		// threading mode checks are not necessary (and do not work) on target
		// platforms that do not have threading (such as webassembly)
		#[cfg(target_arch = "wasm32")]
		fn ensure_safe_sqlite_threading_mode() -> Result<()> {
			Ok(())
		}

		#[cfg(not(any(target_arch = "wasm32")))]
		fn ensure_safe_sqlite_threading_mode() -> Result<()> {
			// Ensure SQLite was compiled in threadsafe mode.
			if unsafe { ffi::sqlite3_threadsafe() == 0 } {
				return Err(Error::SqliteSingleThreadedMode);
			}

			// Now we know SQLite is _capable_ of being in Multi-thread of Serialized mode,
			// but it's possible someone configured it to be in Single-thread mode
			// before calling into us. That would mean we're exposing an unsafe API via
			// a safe one (in Rust terminology).
			//
			// We can ask SQLite for a mutex and check for
			// the magic value 8. This isn't documented, but it's what SQLite
			// returns for its mutex allocation function in Single-thread mode.
			const SQLITE_SINGLETHREADED_MUTEX_MAGIC: usize = 8;
			let is_singlethreaded = unsafe {
				let mutex_ptr = ffi::sqlite3_mutex_alloc(0);
				let is_singlethreaded = mutex_ptr as usize == SQLITE_SINGLETHREADED_MUTEX_MAGIC;
				ffi::sqlite3_mutex_free(mutex_ptr);
				is_singlethreaded
			};
			if is_singlethreaded {
				Err(Error::SqliteSingleThreadedMode)
			} else {
				Ok(())
			}
		}

    } use self::inner_connection::InnerConnection;

	pub mod params
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use crate::{BindIndex, Result, Statement, ToSql};
        */
        mod sealed
		{
			/// This trait exists just to ensure that the only impls of `trait Params`
			/// that are allowed are ones in this crate.
			pub trait Sealed {}
		}
		use sealed::Sealed;

		/// Trait used for [sets of parameter][params] passed into SQL
		/// statements/queries.
		///
		/// [params]: https://www.sqlite.org/c3ref/bind_blob.html
		///
		/// Note: Currently, this trait can only be implemented inside this crate.
		/// Additionally, it's methods (which are `doc(hidden)`) should currently not be
		/// considered part of the stable API, although it's possible they will
		/// stabilize in the future.
		///
		/// # Passing parameters to SQLite
		///
		/// Many functions in this library let you pass parameters to SQLite. Doing this
		/// lets you avoid any risk of SQL injection, and is simpler than escaping
		/// things manually. Aside from deprecated functions and a few helpers, this is
		/// indicated by the function taking a generic argument that implements `Params`
		/// (this trait).
		///
		/// ## Positional parameters
		///
		/// For cases where you want to pass a list of parameters where the number of
		/// parameters is known at compile time, this can be done in one of the
		/// following ways:
		///
		/// - For small lists of parameters up to 16 items, they may alternatively be
		///   passed as a tuple, as in `thing.query((1, "foo"))`.
		///   This is somewhat inconvenient for a single item, since you need a
		///   weird-looking trailing comma: `thing.query(("example",))`. That case is
		///   perhaps more cleanly expressed as `thing.query(["example"])`.
		///
		/// - Using the [`rusqlite::params!`](crate::params!) macro, e.g.
		///   `thing.query(rusqlite::params![1, "foo", bar])`. This is mostly useful for
		///   heterogeneous lists where the number of parameters greater than 16, or
		///   homogeneous lists of parameters where the number of parameters exceeds 32.
		///
		/// - For small homogeneous lists of parameters, they can either be passed as:
		///
		///     - an array, as in `thing.query([1i32, 2, 3, 4])` or `thing.query(["foo",
		///       "bar", "baz"])`.
		///
		///     - a reference to an array of references, as in `thing.query(&["foo",
		///       "bar", "baz"])` or `thing.query(&[&1i32, &2, &3])`.
		///       (Note: in this case we don't implement this for slices for coherence
		///       reasons, so it really is only for the "reference to array" types —
		///       hence why the number of parameters must be <= 32, or you need to
		///       reach for `rusqlite::params!`)
		///
		///   Unfortunately, in the current design it's not possible to allow this for
		///   references to arrays of non-references (e.g. `&[1i32, 2, 3]`). Code like
		///   this should instead either use `params!`, an array literal, a `&[&dyn
		///   ToSql]` or if none of those work, [`ParamsFromIter`].
		///
		/// - As a slice of `ToSql` trait object references, e.g. `&[&dyn ToSql]`. This
		///   is mostly useful for passing parameter lists around as arguments without
		///   having every function take a generic `P: Params`.
		///
		/// ### Example (positional)
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result, params};
		/// fn update_rows(conn: &Connection) -> Result<()> {
		///     let mut stmt = conn.prepare("INSERT INTO test (a, b) VALUES (?1, ?2)")?;
		///
		///     // Using a tuple:
		///     stmt.execute((0, "foobar"))?;
		///
		///     // Using `rusqlite::params!`:
		///     stmt.execute(params![1i32, "blah"])?;
		///
		///     // array literal — non-references
		///     stmt.execute([2i32, 3i32])?;
		///
		///     // array literal — references
		///     stmt.execute(["foo", "bar"])?;
		///
		///     // Slice literal, references:
		///     stmt.execute(&[&2i32, &3i32])?;
		///
		///     // Note: The types behind the references don't have to be `Sized`
		///     stmt.execute(&["foo", "bar"])?;
		///
		///     // However, this doesn't work (see above):
		///     // stmt.execute(&[1i32, 2i32])?;
		///     Ok(())
		/// }
		/// ```
		///
		/// ## Named parameters
		///
		/// SQLite lets you name parameters using a number of conventions (":foo",
		/// "@foo", "$foo"). You can pass named parameters in to SQLite using rusqlite
		/// in a few ways:
		///
		/// - Using the [`rusqlite::named_params!`](crate::named_params!) macro, as in
		///   `stmt.execute(named_params!{ ":name": "foo", ":age": 99 })`. Similar to
		///   the `params` macro, this is most useful for heterogeneous lists of
		///   parameters, or lists where the number of parameters exceeds 32.
		///
		/// - As a slice of `&[(&str, &dyn ToSql)]`. This is what essentially all of
		///   these boil down to in the end, conceptually at least. In theory, you can
		///   pass this as `stmt`.
		///
		/// - As array references, similar to the positional params. This looks like
		///   `thing.query(&[(":foo", &1i32), (":bar", &2i32)])` or
		///   `thing.query(&[(":foo", "abc"), (":bar", "def")])`.
		///
		/// Note: Unbound named parameters will be left to the value they previously
		/// were bound with, falling back to `NULL` for parameters which have never been
		/// bound.
		///
		/// ### Example (named)
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result, named_params};
		/// fn insert(conn: &Connection) -> Result<()> {
		///     let mut stmt = conn.prepare("INSERT INTO test (key, value) VALUES (:key, :val)")?;
		///     // Using `rusqlite::params!`:
		///     stmt.execute(named_params! { ":key": "one", ":val": 2 })?;
		///     // Alternatively:
		///     stmt.execute(&[(":key", "three"), (":val", "four")])?;
		///     // Or:
		///     stmt.execute(&[(":key", &100), (":val", &200)])?;
		///     Ok(())
		/// }
		/// ```
		///
		/// ## No parameters
		///
		/// You can just use an empty tuple or the empty array literal to run a query
		/// that accepts no parameters.
		///
		/// ### Example (no parameters)
		///
		/// The empty tuple:
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result, params};
		/// fn delete_all_users(conn: &Connection) -> Result<()> {
		///     // You may also use `()`.
		///     conn.execute("DELETE FROM users", ())?;
		///     Ok(())
		/// }
		/// ```
		///
		/// The empty array:
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result, params};
		/// fn delete_all_users(conn: &Connection) -> Result<()> {
		///     // Just use an empty array (e.g. `[]`) for no params.
		///     conn.execute("DELETE FROM users", [])?;
		///     Ok(())
		/// }
		/// ```
		///
		/// ## Dynamic parameter list
		///
		/// If you have a number of parameters which is unknown at compile time (for
		/// example, building a dynamic query at runtime), you have two choices:
		///
		/// - Use a `&[&dyn ToSql]`. This is often annoying to construct if you don't
		///   already have this type on-hand.
		/// - Use the [`ParamsFromIter`] type. This essentially lets you wrap an
		///   iterator some `T: ToSql` with something that implements `Params`. The
		///   usage of this looks like `rusqlite::params_from_iter(something)`.
		///
		/// A lot of the considerations here are similar either way, so you should see
		/// the [`ParamsFromIter`] documentation for more info / examples.
		pub trait Params: Sealed {
			// XXX not public api, might not need to expose.
			//
			// Binds the parameters to the statement. It is unlikely calling this
			// explicitly will do what you want. Please use `Statement::query` or
			// similar directly.
			//
			// For now, just hide the function in the docs...
			#[doc(hidden)]
			fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()>;
		}

		// Explicitly impl for empty array. Critically, for `conn.execute([])` to be
		// unambiguous, this must be the *only* implementation for an empty array.
		//
		// This sadly prevents `impl<T: ToSql, const N: usize> Params for [T; N]`, which
		// forces people to use `params![...]` or `rusqlite::params_from_iter` for long
		// homogeneous lists of parameters. This is not that big of a deal, but is
		// unfortunate, especially because I mostly did it because I wanted a simple
		// syntax for no-params that didn't require importing -- the empty tuple fits
		// that nicely, but I didn't think of it until much later.
		//
		// Admittedly, if we did have the generic impl, then we *wouldn't* support the
		// empty array literal as a parameter, since the `T` there would fail to be
		// inferred. The error message here would probably be quite bad, and so on
		// further thought, probably would end up causing *more* surprises, not less.
		impl Sealed for [&(dyn ToSql + Send + Sync); 0] {}
		impl Params for [&(dyn ToSql + Send + Sync); 0] {
			#[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
				stmt.ensure_parameter_count(0)
			}
		}

		impl Sealed for &[&dyn ToSql] {}
		impl Params for &[&dyn ToSql] {
			#[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
				stmt.bind_parameters(self)
			}
		}

		impl<S: BindIndex, T: ToSql> Sealed for &[(S, T)] {}
		impl<S: BindIndex, T: ToSql> Params for &[(S, T)] {
			#[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
				stmt.bind_parameters_named(self)
			}
		}

		// Manual impls for the empty and singleton tuple, although the rest are covered
		// by macros.
		impl Sealed for () {}
		impl Params for () {
			#[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
				stmt.ensure_parameter_count(0)
			}
		}

		// I'm pretty sure you could tweak the `single_tuple_impl` to accept this.
		impl<T: ToSql> Sealed for (T,) {}
		impl<T: ToSql> Params for (T,) {
			#[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
				stmt.ensure_parameter_count(1)?;
				stmt.raw_bind_parameter(1, self.0)?;
				Ok(())
			}
		}

		macro_rules! single_tuple_impl {
			($count:literal : $(($field:tt $ftype:ident)),* $(,)?) => {
				impl<$($ftype,)*> Sealed for ($($ftype,)*) where $($ftype: ToSql,)* {}
				impl<$($ftype,)*> Params for ($($ftype,)*) where $($ftype: ToSql,)* {
					fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
						stmt.ensure_parameter_count($count)?;
						$({
							debug_assert!($field < $count);
							stmt.raw_bind_parameter($field + 1, self.$field)?;
						})+
						Ok(())
					}
				}
			}
		}

		// We use a macro for the rest, but don't bother with trying to implement it
		// in a single invocation (it's possible to do, but my attempts were almost the
		// same amount of code as just writing it out this way, and much more dense --
		// it is a more complicated case than the TryFrom macro we have for row->tuple).
		//
		// Note that going up to 16 (rather than the 12 that the impls in the stdlib
		// usually support) is just because we did the same in the `TryFrom<Row>` impl.
		// I didn't catch that then, but there's no reason to remove it, and it seems
		// nice to be consistent here; this way putting data in the database and getting
		// data out of the database are more symmetric in a (mostly superficial) sense.
		single_tuple_impl!(2: (0 A), (1 B));
		single_tuple_impl!(3: (0 A), (1 B), (2 C));
		single_tuple_impl!(4: (0 A), (1 B), (2 C), (3 D));
		single_tuple_impl!(5: (0 A), (1 B), (2 C), (3 D), (4 E));
		single_tuple_impl!(6: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F));
		single_tuple_impl!(7: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G));
		single_tuple_impl!(8: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H));
		single_tuple_impl!(9: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I));
		single_tuple_impl!(10: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J));
		single_tuple_impl!(11: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K));
		single_tuple_impl!(12: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L));
		single_tuple_impl!(13: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M));
		single_tuple_impl!(14: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M), (13 N));
		single_tuple_impl!(15: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M), (13 N), (14 O));
		single_tuple_impl!(16: (0 A), (1 B), (2 C), (3 D), (4 E), (5 F), (6 G), (7 H), (8 I), (9 J), (10 K), (11 L), (12 M), (13 N), (14 O), (15 P));

		macro_rules! impl_for_array_ref {
			($($N:literal)+) => {$(
				// These are already generic, and there's a shedload of them, so lets
				// avoid the compile time hit from making them all inline for now.
				impl<T: ToSql + ?Sized> Sealed for &[&T; $N] {}
				impl<T: ToSql + ?Sized> Params for &[&T; $N] {
					fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
						stmt.bind_parameters(self)
					}
				}
				impl<S: BindIndex, T: ToSql + ?Sized> Sealed for &[(S, &T); $N] {}
				impl<S: BindIndex, T: ToSql + ?Sized> Params for &[(S, &T); $N] {
					fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
						stmt.bind_parameters_named(self)
					}
				}
				impl<T: ToSql> Sealed for [T; $N] {}
				impl<T: ToSql> Params for [T; $N] {
					#[inline]
					fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
						stmt.bind_parameters(&self)
					}
				}
			)+};
		}

		// Following libstd/libcore's (old) lead, implement this for arrays up to `[_;
		// 32]`. Note `[_; 0]` is intentionally omitted for coherence reasons, see the
		// note above the impl of `[&dyn ToSql; 0]` for more information.
		//
		// Note that this unfortunately means we can't use const generics here, but I
		// don't really think it matters -- users who hit that can use `params!` anyway.
		impl_for_array_ref!(
			1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
			18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
		);

		/// Adapter type which allows any iterator over [`ToSql`] values to implement
		/// [`Params`].
		///
		/// This struct is created by the [`params_from_iter`] function.
		///
		/// This can be useful if you have something like an `&[String]` (of unknown
		/// length), and you want to use them with an API that wants something
		/// implementing `Params`. This way, you can avoid having to allocate storage
		/// for something like a `&[&dyn ToSql]`.
		///
		/// This essentially is only ever actually needed when dynamically generating
		/// SQL — static SQL (by definition) has the number of parameters known
		/// statically. As dynamically generating SQL is itself pretty advanced, this
		/// API is itself for advanced use cases (See "Realistic use case" in the
		/// examples).
		///
		/// # Example
		///
		/// ## Basic usage
		///
		/// ```rust,no_run
		/// use rusqlite::{params_from_iter, Connection, Result};
		/// use std::collections::BTreeSet;
		///
		/// fn query(conn: &Connection, ids: &BTreeSet<String>) -> Result<()> {
		///     assert_eq!(ids.len(), 3, "Unrealistic sample code");
		///
		///     let mut stmt = conn.prepare("SELECT * FROM users WHERE id IN (?1, ?2, ?3)")?;
		///     let _rows = stmt.query(params_from_iter(ids.iter()))?;
		///
		///     // use _rows...
		///     Ok(())
		/// }
		/// ```
		///
		/// ## Realistic use case
		///
		/// Here's how you'd use `ParamsFromIter` to call [`Statement::exists`] with a
		/// dynamic number of parameters.
		///
		/// ```rust,no_run
		/// use rusqlite::{Connection, Result};
		///
		/// pub fn any_active_users(conn: &Connection, usernames: &[String]) -> Result<bool> {
		///     if usernames.is_empty() {
		///         return Ok(false);
		///     }
		///
		///     // Note: `repeat_vars` never returns anything attacker-controlled, so
		///     // it's fine to use it in a dynamically-built SQL string.
		///     let vars = repeat_vars(usernames.len());
		///
		///     let sql = format!(
		///         // In practice this would probably be better as an `EXISTS` query.
		///         "SELECT 1 FROM user WHERE is_active AND name IN ({}) LIMIT 1",
		///         vars,
		///     );
		///     let mut stmt = conn.prepare(&sql)?;
		///     stmt.exists(rusqlite::params_from_iter(usernames))
		/// }
		///
		/// // Helper function to return a comma-separated sequence of `?`.
		/// // - `repeat_vars(0) => panic!(...)`
		/// // - `repeat_vars(1) => "?"`
		/// // - `repeat_vars(2) => "?,?"`
		/// // - `repeat_vars(3) => "?,?,?"`
		/// // - ...
		/// fn repeat_vars(count: usize) -> String {
		///     assert_ne!(count, 0);
		///     let mut s = "?,".repeat(count);
		///     // Remove trailing comma
		///     s.pop();
		///     s
		/// }
		/// ```
		///
		/// That is fairly complex, and even so would need even more work to be fully
		/// production-ready:
		///
		/// - production code should ensure `usernames` isn't so large that it will
		///   surpass [`conn.limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER)`][limits],
		///   chunking if too large. (Note that the limits api requires rusqlite to have
		///   the "limits" feature).
		///
		/// - `repeat_vars` can be implemented in a way that avoids needing to allocate
		///   a String.
		///
		/// - Etc...
		///
		/// [limits]: crate::Connection::limit
		///
		/// This complexity reflects the fact that `ParamsFromIter` is mainly intended
		/// for advanced use cases — most of the time you should know how many
		/// parameters you have statically (and if you don't, you're either doing
		/// something tricky, or should take a moment to think about the design).
		#[derive(Clone, Debug)]
		pub struct ParamsFromIter<I>(I);

		/// Constructor function for a [`ParamsFromIter`]. See its documentation for
		/// more.
		#[inline]
		pub fn params_from_iter<I>(iter: I) -> ParamsFromIter<I>
		where
			I: IntoIterator,
			I::Item: ToSql,
		{
			ParamsFromIter(iter)
		}

		impl<I> Sealed for ParamsFromIter<I>
		where
			I: IntoIterator,
			I::Item: ToSql,
		{
		}

		impl<I> Params for ParamsFromIter<I>
		where
			I: IntoIterator,
			I::Item: ToSql,
		{
			#[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> {
				stmt.bind_parameters(self.0)
			}
		}

    } pub use self::params::{params_from_iter, Params, ParamsFromIter};

	pub mod pragma
    {
        /*!
        Pragma helpers */
        use ::
        {
            *,
        };
        /*
		use std::ops::Deref;

		use crate::ffi;
		use crate::types::{ToSql, ToSqlOutput, ValueRef};
		use crate::{Connection, Result, Row};
        */
        pub struct Sql {
			buf: String,
		}

		impl Sql {
			pub fn new() -> Self {
				Self { buf: String::new() }
			}

			pub fn push_pragma(&mut self, schema_name: Option<&str>, pragma_name: &str) -> Result<()> {
				self.push_keyword("PRAGMA")?;
				self.push_space();
				if let Some(schema_name) = schema_name {
					self.push_schema_name(schema_name);
					self.push_dot();
				}
				self.push_keyword(pragma_name)
			}

			pub fn push_keyword(&mut self, keyword: &str) -> Result<()> {
				if !keyword.is_empty() && is_identifier(keyword) {
					self.buf.push_str(keyword);
					Ok(())
				} else {
					Err(err!(ffi::SQLITE_MISUSE, "Invalid keyword \"{keyword}\""))
				}
			}

			pub fn push_schema_name(&mut self, schema_name: &str) {
				self.push_identifier(schema_name);
			}

			pub fn push_identifier(&mut self, s: &str) {
				if is_identifier(s) {
					self.buf.push_str(s);
				} else {
					self.wrap_and_escape(s, '"');
				}
			}

			pub fn push_value(&mut self, value: &dyn ToSql) -> Result<()>
            {
				let value = value.to_sql()?;
				let value = match value {
					ToSqlOutput::Borrowed(v) => v,
					ToSqlOutput::Owned(ref v) => ValueRef::from(v),
					#[cfg(any(feature = "blob", feature = "functions", feature = "array"))]
					_ => {
						return Err(err!(ffi::SQLITE_MISUSE, "Unsupported value \"{value:?}\""));
					}
				};
				match value {
					ValueRef::Integer(i) => {
						self.push_int(i);
					}
					ValueRef::Real(r) => {
						self.push_real(r);
					}
					ValueRef::Text(s) => {
						let s = std::str::from_utf8(s)?;
						self.push_string_literal(s);
					}
					_ => {
						return Err(err!(ffi::SQLITE_MISUSE, "Unsupported value \"{value:?}\""));
					}
				};
				Ok(())
			}

			pub fn push_string_literal(&mut self, s: &str) {
				self.wrap_and_escape(s, '\'');
			}

			pub fn push_int(&mut self, i: i64) {
				self.buf.push_str(&i.to_string());
			}

			pub fn push_real(&mut self, f: f64) {
				self.buf.push_str(&f.to_string());
			}

			pub fn push_space(&mut self) {
				self.buf.push(' ');
			}

			pub fn push_dot(&mut self) {
				self.buf.push('.');
			}

			pub fn push_equal_sign(&mut self) {
				self.buf.push('=');
			}

			pub fn open_brace(&mut self) {
				self.buf.push('(');
			}

			pub fn close_brace(&mut self) {
				self.buf.push(')');
			}

			pub fn as_str(&self) -> &str {
				&self.buf
			}

			fn wrap_and_escape(&mut self, s: &str, quote: char) {
				self.buf.push(quote);
				let chars = s.chars();
				for ch in chars {
					// escape `quote` by doubling it
					if ch == quote {
						self.buf.push(ch);
					}
					self.buf.push(ch);
				}
				self.buf.push(quote);
			}
		}

		impl Deref for Sql {
			type Target = str;

			fn deref(&self) -> &str {
				self.as_str()
			}
		}

		impl Connection {
			/// Query the current value of `pragma_name`.
			///
			/// Some pragmas will return multiple rows/values which cannot be retrieved
			/// with this method.
			///
			/// Prefer [PRAGMA function](https://sqlite.org/pragma.html#pragfunc) introduced in SQLite 3.20:
			/// `SELECT user_version FROM pragma_user_version;`
			pub fn pragma_query_value<T, F>(
				&self,
				schema_name: Option<&str>,
				pragma_name: &str,
				f: F,
			) -> Result<T>
			where
				F: FnOnce(&Row<'_>) -> Result<T>,
			{
				let mut query = Sql::new();
				query.push_pragma(schema_name, pragma_name)?;
				self.query_row(&query, [], f)
			}
			/// Query the current rows/values of `pragma_name`.
			///
			/// Prefer [PRAGMA function](https://sqlite.org/pragma.html#pragfunc) introduced in SQLite 3.20:
			/// `SELECT * FROM pragma_collation_list;`
			pub fn pragma_query<F>(
				&self,
				schema_name: Option<&str>,
				pragma_name: &str,
				mut f: F,
			) -> Result<()>
			where
				F: FnMut(&Row<'_>) -> Result<()>,
			{
				let mut query = Sql::new();
				query.push_pragma(schema_name, pragma_name)?;
				let mut stmt = self.prepare(&query)?;
				let mut rows = stmt.query([])?;
				while let Some(result_row) = rows.next()? {
					let row = result_row;
					f(row)?;
				}
				Ok(())
			}
			/// Query the current value(s) of `pragma_name` associated to
			/// `pragma_value`.
			///
			/// This method can be used with query-only pragmas which need an argument
			/// (e.g. `table_info('one_tbl')`) or pragmas which returns value(s)
			/// (e.g. `integrity_check`).
			///
			/// Prefer [PRAGMA function](https://sqlite.org/pragma.html#pragfunc) introduced in SQLite 3.20:
			/// `SELECT * FROM pragma_table_info(?1);`
			pub fn pragma<F, V>(
				&self,
				schema_name: Option<&str>,
				pragma_name: &str,
				pragma_value: V,
				mut f: F,
			) -> Result<()>
			where
				F: FnMut(&Row<'_>) -> Result<()>,
				V: ToSql,
			{
				let mut sql = Sql::new();
				sql.push_pragma(schema_name, pragma_name)?;
				// The argument may be either in parentheses
				// or it may be separated from the pragma name by an equal sign.
				// The two syntaxes yield identical results.
				sql.open_brace();
				sql.push_value(&pragma_value)?;
				sql.close_brace();
				let mut stmt = self.prepare(&sql)?;
				let mut rows = stmt.query([])?;
				while let Some(result_row) = rows.next()? {
					let row = result_row;
					f(row)?;
				}
				Ok(())
			}
			/// Set a new value to `pragma_name`.
			///
			/// Some pragmas will return the updated value which cannot be retrieved
			/// with this method.
			pub fn pragma_update<V>(
				&self,
				schema_name: Option<&str>,
				pragma_name: &str,
				pragma_value: V,
			) -> Result<()>
			where
				V: ToSql,
			{
				let mut sql = Sql::new();
				sql.push_pragma(schema_name, pragma_name)?;
				// The argument may be either in parentheses
				// or it may be separated from the pragma name by an equal sign.
				// The two syntaxes yield identical results.
				sql.push_equal_sign();
				sql.push_value(&pragma_value)?;
				self.execute_batch(&sql)
			}
			/// Set a new value to `pragma_name` and return the updated value.
			///
			/// Only few pragmas automatically return the updated value.
			pub fn pragma_update_and_check<F, T, V>(
				&self,
				schema_name: Option<&str>,
				pragma_name: &str,
				pragma_value: V,
				f: F,
			) -> Result<T>
			where
				F: FnOnce(&Row<'_>) -> Result<T>,
				V: ToSql,
			{
				let mut sql = Sql::new();
				sql.push_pragma(schema_name, pragma_name)?;
				// The argument may be either in parentheses
				// or it may be separated from the pragma name by an equal sign.
				// The two syntaxes yield identical results.
				sql.push_equal_sign();
				sql.push_value(&pragma_value)?;
				self.query_row(&sql, [], f)
			}
		}

		fn is_identifier(s: &str) -> bool {
			let chars = s.char_indices();
			for (i, ch) in chars {
				if i == 0 {
					if !is_identifier_start(ch) {
						return false;
					}
				} else if !is_identifier_continue(ch) {
					return false;
				}
			}
			true
		}

		fn is_identifier_start(c: char) -> bool {
			c.is_ascii_uppercase() || c == '_' || c.is_ascii_lowercase() || c > '\x7F'
		}

		fn is_identifier_continue(c: char) -> bool {
			c == '$'
				|| c.is_ascii_digit()
				|| c.is_ascii_uppercase()
				|| c == '_'
				|| c.is_ascii_lowercase()
				|| c > '\x7F'
		}
    }

	pub mod row
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use fallible_iterator::FallibleIterator;
		use fallible_streaming_iterator::FallibleStreamingIterator;
		use std::convert;

		use super::{Error, Result, Statement};
		use crate::types::{FromSql, FromSqlError, ValueRef};
        */
		/// A handle (lazy fallible streaming iterator) for the resulting rows of a query.
		#[must_use = "Rows is lazy and will do nothing unless consumed"]
		pub struct Rows<'stmt> {
			pub(crate) stmt: Option<&'stmt Statement<'stmt>>,
			row: Option<Row<'stmt>>,
		}

		impl<'stmt> Rows<'stmt> {
			#[inline] fn reset(&mut self) -> Result<()> {
				if let Some(stmt) = self.stmt.take() {
					stmt.reset()
				} else {
					Ok(())
				}
			}
			/// Attempt to get the next row from the query. Returns `Ok(Some(Row))` if
			/// there is another row, `Err(...)` if there was an error
			/// getting the next row, and `Ok(None)` if all rows have been retrieved.
			///
			/// ## Note
			///
			/// This interface is not compatible with Rust's `Iterator` trait, because
			/// the lifetime of the returned row is tied to the lifetime of `self`.
			/// This is a fallible "streaming iterator". For a more natural interface,
			/// consider using [`query_map`](Statement::query_map) or
			/// [`query_and_then`](Statement::query_and_then) instead, which
			/// return types that implement `Iterator`.
			#[expect(clippy::should_implement_trait)] // cannot implement Iterator
			#[inline] pub fn next(&mut self) -> Result<Option<&Row<'stmt>>> {
				self.advance()?;
				Ok((*self).get())
			}
			/// Map over this `Rows`, converting it to a [`Map`], which
			/// implements `FallibleIterator`.
			/// ```rust,no_run
			/// use fallible_iterator::FallibleIterator;
			/// # use rusqlite::{Result, Statement};
			/// fn query(stmt: &mut Statement) -> Result<Vec<i64>> {
			///     let rows = stmt.query([])?;
			///     rows.map(|r| r.get(0)).collect()
			/// }
			/// ```
			// FIXME Hide FallibleStreamingIterator::map
			#[inline] pub fn map<F, B>(self, f: F) -> Map<'stmt, F>
			where
				F: FnMut(&Row<'_>) -> Result<B>,
			{
				Map { rows: self, f }
			}
			/// Map over this `Rows`, converting it to a [`MappedRows`], which
			/// implements `Iterator`.
			#[inline] pub fn mapped<F, B>(self, f: F) -> MappedRows<'stmt, F>
			where
				F: FnMut(&Row<'_>) -> Result<B>,
			{
				MappedRows { rows: self, map: f }
			}
			/// Map over this `Rows` with a fallible function, converting it to a
			/// [`AndThenRows`], which implements `Iterator` (instead of
			/// `FallibleStreamingIterator`).
			#[inline] pub fn and_then<F, T, E>(self, f: F) -> AndThenRows<'stmt, F>
			where
				F: FnMut(&Row<'_>) -> Result<T, E>,
			{
				AndThenRows { rows: self, map: f }
			}
			/// Give access to the underlying statement
			#[must_use]
			pub fn as_ref(&self) -> Option<&Statement<'stmt>> {
				self.stmt
			}
		}

		impl<'stmt> Rows<'stmt> {
			#[inline]
			pub(crate) fn new(stmt: &'stmt Statement<'stmt>) -> Self {
				Rows {
					stmt: Some(stmt),
					row: None,
				}
			}

			#[inline]
			pub(crate) fn get_expected_row(&mut self) -> Result<&Row<'stmt>> {
				match self.next()? {
					Some(row) => Ok(row),
					None => Err(Error::QueryReturnedNoRows),
				}
			}
		}

		impl Drop for Rows<'_> {
			#[expect(unused_must_use)]
			#[inline] fn drop(&mut self) {
				self.reset();
			}
		}

		/// `F` is used to transform the _streaming_ iterator into a _fallible_
		/// iterator.
		#[must_use = "iterators are lazy and do nothing unless consumed"]
		pub struct Map<'stmt, F> {
			rows: Rows<'stmt>,
			f: F,
		}

		impl<F, B> FallibleIterator for Map<'_, F>
		where
			F: FnMut(&Row<'_>) -> Result<B>,
		{
			type Error = Error;
			type Item = B;

			#[inline] fn next(&mut self) -> Result<Option<B>> {
				match self.rows.next()? {
					Some(v) => Ok(Some((self.f)(v)?)),
					None => Ok(None),
				}
			}
		}

		/// An iterator over the mapped resulting rows of a query.
		///
		/// `F` is used to transform the _streaming_ iterator into a _standard_
		/// iterator.
		#[must_use = "iterators are lazy and do nothing unless consumed"]
		pub struct MappedRows<'stmt, F> {
			rows: Rows<'stmt>,
			map: F,
		}

		impl<T, F> Iterator for MappedRows<'_, F>
		where
			F: FnMut(&Row<'_>) -> Result<T>,
		{
			type Item = Result<T>;

			#[inline] fn next(&mut self) -> Option<Result<T>>
            {
				let map = &mut self.map;
				self.rows
					.next()
					.transpose()
					.map(|row_result| row_result.and_then(map))
			}
		}

		/// An iterator over the mapped resulting rows of a query, with an Error type
		/// unifying with Error.
		#[must_use = "iterators are lazy and do nothing unless consumed"]
		pub struct AndThenRows<'stmt, F> {
			rows: Rows<'stmt>,
			map: F,
		}

		impl<T, E, F> Iterator for AndThenRows<'_, F>
		where
			E: From<Error>,
			F: FnMut(&Row<'_>) -> Result<T, E>,
		{
			type Item = Result<T, E>;

			#[inline] fn next(&mut self) -> Option<Self::Item>
            {
				let map = &mut self.map;
				self.rows
					.next()
					.transpose()
					.map(|row_result| row_result.map_err(E::from).and_then(map))
			}
		}

		/// `FallibleStreamingIterator` differs from the standard library's `Iterator`
		/// in two ways:
		/// * each call to `next` (`sqlite3_step`) can fail.
		/// * returned `Row` is valid until `next` is called again or `Statement` is
		///   reset or finalized.
		///
		/// While these iterators cannot be used with Rust `for` loops, `while let`
		/// loops offer a similar level of ergonomics:
		/// ```rust,no_run
		/// # use rusqlite::{Result, Statement};
		/// fn query(stmt: &mut Statement) -> Result<()> {
		///     let mut rows = stmt.query([])?;
		///     while let Some(row) = rows.next()? {
		///         // scan columns value
		///     }
		///     Ok(())
		/// }
		/// ```
		impl<'stmt> FallibleStreamingIterator for Rows<'stmt> {
			type Error = Error;
			type Item = Row<'stmt>;

			#[inline] fn advance(&mut self) -> Result<()> {
				if let Some(stmt) = self.stmt {
					match stmt.step() {
						Ok(true) => {
							self.row = Some(Row { stmt });
							Ok(())
						}
						Ok(false) => {
							let r = self.reset();
							self.row = None;
							r
						}
						Err(e) => {
							let _ = self.reset(); // prevents infinite loop on error
							self.row = None;
							Err(e)
						}
					}
				} else {
					self.row = None;
					Ok(())
				}
			}

			#[inline] fn get(&self) -> Option<&Row<'stmt>> {
				self.row.as_ref()
			}
		}

		/// A single result row of a query.
		pub struct Row<'stmt> {
			pub(crate) stmt: &'stmt Statement<'stmt>,
		}

		impl Row<'_> {
			/// Get the value of a particular column of the result row.
			///
			/// # Panics
			///
			/// Panics if calling [`row.get(idx)`](Row::get) would return an error,
			/// including:
			///
			/// * If the underlying SQLite column type is not a valid type as a source
			///   for `T`
			/// * If the underlying SQLite integral value is outside the range
			///   representable by `T`
			/// * If `idx` is outside the range of columns in the returned query
			#[track_caller]
			pub fn get_unwrap<I: RowIndex, T: FromSql>(&self, idx: I) -> T {
				self.get(idx).unwrap()
			}
			/// Get the value of a particular column of the result row.
			///
			/// ## Failure
			///
			/// Returns an `Error::InvalidColumnType` if the underlying SQLite column
			/// type is not a valid type as a source for `T`.
			///
			/// Returns an `Error::InvalidColumnIndex` if `idx` is outside the valid
			/// column range for this row.
			///
			/// Returns an `Error::InvalidColumnName` if `idx` is not a valid column
			/// name for this row.
			///
			/// If the result type is i128 (which requires the `i128_blob` feature to be
			/// enabled), and the underlying SQLite column is a blob whose size is not
			/// 16 bytes, `Error::InvalidColumnType` will also be returned.
			#[track_caller]
			pub fn get<I: RowIndex, T: FromSql>(&self, idx: I) -> Result<T>
            {
				let idx = idx.idx(self.stmt)?;
				let value = self.stmt.value_ref(idx);
				FromSql::column_result(value).map_err(|err| match err {
					FromSqlError::InvalidType => Error::InvalidColumnType(
						idx,
						self.stmt.column_name_unwrap(idx).into(),
						value.data_type(),
					),
					FromSqlError::OutOfRange(i) => Error::IntegralValueOutOfRange(idx, i),
					FromSqlError::Other(err) => {
						Error::FromSqlConversionFailure(idx, value.data_type(), err)
					}
					FromSqlError::InvalidBlobSize { .. } => {
						Error::FromSqlConversionFailure(idx, value.data_type(), Box::new(err))
					}
				})
			}
			/// Get the value of a particular column of the result row as a `ValueRef`,
			/// allowing data to be read out of a row without copying.
			///
			/// This `ValueRef` is valid only as long as this Row, which is enforced by
			/// its lifetime. This means that while this method is completely safe,
			/// it can be somewhat difficult to use, and most callers will be better
			/// served by [`get`](Row::get) or [`get_unwrap`](Row::get_unwrap).
			///
			/// ## Failure
			///
			/// Returns an `Error::InvalidColumnIndex` if `idx` is outside the valid
			/// column range for this row.
			///
			/// Returns an `Error::InvalidColumnName` if `idx` is not a valid column
			/// name for this row.
			pub fn get_ref<I: RowIndex>(&self, idx: I) -> Result<ValueRef<'_>>
            {
				let idx = idx.idx(self.stmt)?;
				// Narrowing from `ValueRef<'stmt>` (which `self.stmt.value_ref(idx)`
				// returns) to `ValueRef<'a>` is needed because it's only valid until
				// the next call to sqlite3_step.
				let val_ref = self.stmt.value_ref(idx);
				Ok(val_ref)
			}
			/// Get the value of a particular column of the result row as a `ValueRef`,
			/// allowing data to be read out of a row without copying.
			///
			/// This `ValueRef` is valid only as long as this Row, which is enforced by
			/// its lifetime. This means that while this method is completely safe,
			/// it can be difficult to use, and most callers will be better served by
			/// [`get`](Row::get) or [`get_unwrap`](Row::get_unwrap).
			///
			/// # Panics
			///
			/// Panics if calling [`row.get_ref(idx)`](Row::get_ref) would return an
			/// error, including:
			///
			/// * If `idx` is outside the range of columns in the returned query.
			/// * If `idx` is not a valid column name for this row.
			#[track_caller]
			pub fn get_ref_unwrap<I: RowIndex>(&self, idx: I) -> ValueRef<'_> {
				self.get_ref(idx).unwrap()
			}
		}

		impl<'stmt> AsRef<Statement<'stmt>> for Row<'stmt> {
			fn as_ref(&self) -> &Statement<'stmt> {
				self.stmt
			}
		}

		/// Debug `Row` like an ordered `Map<Result<&str>, Result<(Type, ValueRef)>>`
		/// with column name as key except that for `Type::Blob` only its size is
		/// printed (not its content).
		impl std::fmt::Debug for Row<'_> {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				let mut dm = f.debug_map();
				for c in 0..self.stmt.column_count() {
					let name = self.stmt.column_name(c).expect("valid column index");
					dm.key(&name);
					let value = self.get_ref(c);
					match value {
						Ok(value) => {
							let dt = value.data_type();
							match value {
								ValueRef::Null => {
									dm.value(&(dt, ()));
								}
								ValueRef::Integer(i) => {
									dm.value(&(dt, i));
								}
								ValueRef::Real(f) => {
									dm.value(&(dt, f));
								}
								ValueRef::Text(s) => {
									dm.value(&(dt, String::from_utf8_lossy(s)));
								}
								ValueRef::Blob(b) => {
									dm.value(&(dt, b.len()));
								}
							}
						}
						Err(ref _err) => {
							dm.value(&value);
						}
					}
				}
				dm.finish()
			}
		}

		mod sealed {
			/// This trait exists just to ensure that the only impls of `trait RowIndex`
			/// that are allowed are ones in this crate.
			pub trait Sealed {}
			impl Sealed for usize {}
			impl Sealed for &str {}
		}

		/// A trait implemented by types that can index into columns of a row.
		///
		/// It is only implemented for `usize` and `&str`.
		pub trait RowIndex: sealed::Sealed {
			/// Returns the index of the appropriate column, or `Error` if no such
			/// column exists.
			fn idx(&self, stmt: &Statement<'_>) -> Result<usize>;
		}

		impl RowIndex for usize {
			#[inline] fn idx(&self, stmt: &Statement<'_>) -> Result<usize> {
				if *self >= stmt.column_count() {
					Err(Error::InvalidColumnIndex(*self))
				} else {
					Ok(*self)
				}
			}
		}

		impl RowIndex for &'_ str {
			#[inline] fn idx(&self, stmt: &Statement<'_>) -> Result<usize> {
				stmt.column_index(self)
			}
		}

		macro_rules! tuple_try_from_row {
			($($field:ident),*) => {
				impl<'a, $($field,)*> convert::TryFrom<&'a Row<'a>> for ($($field,)*) where $($field: FromSql,)* {
					type Error = crate::Error;

					// we end with index += 1, which rustc warns about
					// unused_variables and unused_mut are allowed for ()
					#[allow(unused_assignments, unused_variables, unused_mut)]
					fn try_from(row: &'a Row<'a>) -> Result<Self> {
						let mut index = 0;
						$(
							#[expect(non_snake_case)]
							let $field = row.get::<_, $field>(index)?;
							index += 1;
						)*
						Ok(($($field,)*))
					}
				}
			}
		}

		macro_rules! tuples_try_from_row {
			() => {
				// not very useful, but maybe some other macro users will find this helpful
				tuple_try_from_row!();
			};
			($first:ident $(, $remaining:ident)*) => {
				tuple_try_from_row!($first $(, $remaining)*);
				tuples_try_from_row!($($remaining),*);
			};
		}

		tuples_try_from_row!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

    } pub use self::row::{AndThenRows, Map, MappedRows, Row, RowIndex, Rows};

	pub mod statement
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use std::ffi::{c_int, c_void};
		#[cfg(feature = "array")]
		use std::rc::Rc;
		use std::slice::from_raw_parts;
		use std::{fmt, mem, ptr, str};

		use super::ffi;
		use super::{len_as_c_int, str_for_sqlite};
		use super::{
			AndThenRows, Connection, Error, MappedRows, Params, RawStatement, Result, Row, Rows, ValueRef,
		};
		use crate::bind::BindIndex;
		use crate::types::{ToSql, ToSqlOutput};
		#[cfg(feature = "array")]
		use crate::vtab::array::{free_array, ARRAY_TYPE};

        */
		//pub mod raw_statement
		pub mod raw
		{
			/*!
			*/
			use ::
			{
				*,
			};
			/*
			use super::ffi;
			use super::StatementStatus;
			use crate::util::ParamIndexCache;
			use crate::util::SqliteMallocString;
			use std::ffi::{c_int, CStr};
			use std::ptr;
			use std::sync::Arc;
			*/
			/// Private newtype for raw sqlite3_stmts that finalize themselves when dropped.
			#[derive(Debug)]
			pub struct RawStatement
			{
				ptr: *mut ffi::sqlite3_stmt,
				// Cached indices of named parameters, computed on the fly.
				cache: ParamIndexCache,
				// Cached SQL (trimmed) that we use as the key when we're in the statement
				// cache. This is None for statements which didn't come from the statement
				// cache.
				//
				// This is probably the same as `self.sql()` in most cases, but we don't
				// care either way -- It's a better cache key as it is anyway since it's the
				// actual source we got from rust.
				//
				// One example of a case where the result of `sqlite_sql` and the value in
				// `statement_cache_key` might differ is if the statement has a `tail`.
				statement_cache_key: Option<Arc<str>>,
			}

			impl RawStatement {
				#[inline]
				pub unsafe fn new(stmt: *mut ffi::sqlite3_stmt) -> Self {
					Self {
						ptr: stmt,
						cache: ParamIndexCache::default(),
						statement_cache_key: None,
					}
				}

				#[inline]
				pub fn is_null(&self) -> bool {
					self.ptr.is_null()
				}

				#[inline]
				pub(crate) fn set_statement_cache_key(&mut self, p: impl Into<Arc<str>>) {
					self.statement_cache_key = Some(p.into());
				}

				#[inline]
				pub(crate) fn statement_cache_key(&self) -> Option<Arc<str>> {
					self.statement_cache_key.clone()
				}

				#[inline]
				pub unsafe fn ptr(&self) -> *mut ffi::sqlite3_stmt {
					self.ptr
				}

				#[inline]
				pub fn column_count(&self) -> usize {
					// Note: Can't cache this as it changes if the schema is altered.
					unsafe { ffi::sqlite3_column_count(self.ptr) as usize }
				}

				#[inline]
				pub fn column_type(&self, idx: usize) -> c_int {
					unsafe { ffi::sqlite3_column_type(self.ptr, idx as c_int) }
				}

				#[inline]
				#[cfg(feature = "column_metadata")]
				pub fn column_database_name(&self, idx: usize) -> Option<&CStr> {
					unsafe {
						let db_name = ffi::sqlite3_column_database_name(self.ptr, idx as c_int);
						if db_name.is_null() {
							None
						} else {
							Some(CStr::from_ptr(db_name))
						}
					}
				}

				#[inline]
				#[cfg(feature = "column_metadata")]
				pub fn column_table_name(&self, idx: usize) -> Option<&CStr> {
					unsafe {
						let tbl_name = ffi::sqlite3_column_table_name(self.ptr, idx as c_int);
						if tbl_name.is_null() {
							None
						} else {
							Some(CStr::from_ptr(tbl_name))
						}
					}
				}

				#[inline]
				#[cfg(feature = "column_metadata")]
				pub fn column_origin_name(&self, idx: usize) -> Option<&CStr> {
					unsafe {
						let origin_name = ffi::sqlite3_column_origin_name(self.ptr, idx as c_int);
						if origin_name.is_null() {
							None
						} else {
							Some(CStr::from_ptr(origin_name))
						}
					}
				}

				#[inline]
				#[cfg(feature = "column_decltype")]
				pub fn column_decltype(&self, idx: usize) -> Option<&CStr> {
					unsafe {
						let decltype = ffi::sqlite3_column_decltype(self.ptr, idx as c_int);
						if decltype.is_null() {
							None
						} else {
							Some(CStr::from_ptr(decltype))
						}
					}
				}

				#[inline]
				pub fn column_name(&self, idx: usize) -> Option<&CStr> {
					let idx = idx as c_int;
					if idx < 0 || idx >= self.column_count() as c_int {
						return None;
					}
					unsafe {
						let ptr = ffi::sqlite3_column_name(self.ptr, idx);
						// If ptr is null here, it's an OOM, so there's probably nothing
						// meaningful we can do. Just assert instead of returning None.
						assert!(
							!ptr.is_null(),
							"Null pointer from sqlite3_column_name: Out of memory?"
						);
						Some(CStr::from_ptr(ptr))
					}
				}

				#[inline]
				#[cfg(not(feature = "unlock_notify"))]
				pub fn step(&self) -> c_int {
					unsafe { ffi::sqlite3_step(self.ptr) }
				}

				#[cfg(feature = "unlock_notify")]
				pub fn step(&self) -> c_int {
					use crate::unlock_notify;
					let mut db = ptr::null_mut::<ffi::sqlite3>();
					loop {
						unsafe {
							let mut rc = ffi::sqlite3_step(self.ptr);
							// Bail out early for success and errors unrelated to locking. We
							// still need check `is_locked` after this, but checking now lets us
							// avoid one or two (admittedly cheap) calls into SQLite that we
							// don't need to make.
							if (rc & 0xff) != ffi::SQLITE_LOCKED {
								break rc;
							}
							if db.is_null() {
								db = ffi::sqlite3_db_handle(self.ptr);
							}
							if !unlock_notify::is_locked(db, rc) {
								break rc;
							}
							rc = unlock_notify::wait_for_unlock_notify(db);
							if rc != ffi::SQLITE_OK {
								break rc;
							}
							self.reset();
						}
					}
				}

				#[inline]
				pub fn reset(&self) -> c_int {
					unsafe { ffi::sqlite3_reset(self.ptr) }
				}

				#[inline]
				pub fn bind_parameter_count(&self) -> usize {
					unsafe { ffi::sqlite3_bind_parameter_count(self.ptr) as usize }
				}

				#[inline]
				pub fn bind_parameter_index(&self, name: &str) -> Option<usize> {
					self.cache.get_or_insert_with(name, |param_cstr| {
						let r = unsafe { ffi::sqlite3_bind_parameter_index(self.ptr, param_cstr.as_ptr()) };
						match r {
							0 => None,
							i => Some(i as usize),
						}
					})
				}

				#[inline]
				pub fn bind_parameter_name(&self, index: i32) -> Option<&CStr> {
					unsafe {
						let name = ffi::sqlite3_bind_parameter_name(self.ptr, index);
						if name.is_null() {
							None
						} else {
							Some(CStr::from_ptr(name))
						}
					}
				}

				#[inline]
				pub fn clear_bindings(&mut self) {
					unsafe {
						ffi::sqlite3_clear_bindings(self.ptr);
					} // rc is always SQLITE_OK
				}

				#[inline]
				pub fn sql(&self) -> Option<&CStr> {
					if self.ptr.is_null() {
						None
					} else {
						Some(unsafe { CStr::from_ptr(ffi::sqlite3_sql(self.ptr)) })
					}
				}

				#[inline]
				pub fn finalize(mut self) -> c_int {
					self.finalize_()
				}

				#[inline]
				fn finalize_(&mut self) -> c_int {
					let r = unsafe { ffi::sqlite3_finalize(self.ptr) };
					self.ptr = ptr::null_mut();
					r
				}

				// does not work for PRAGMA
				#[inline]
				pub fn readonly(&self) -> bool {
					unsafe { ffi::sqlite3_stmt_readonly(self.ptr) != 0 }
				}

				#[inline]
				pub(crate) fn expanded_sql(&self) -> Option<SqliteMallocString> {
					unsafe { expanded_sql(self.ptr) }
				}

				#[inline]
				pub fn get_status(&self, status: StatementStatus, reset: bool) -> i32 {
					unsafe { stmt_status(self.ptr, status, reset) }
				}

				#[inline]
				#[cfg(feature = "modern_sqlite")] // 3.28.0
				pub fn is_explain(&self) -> i32 {
					unsafe { ffi::sqlite3_stmt_isexplain(self.ptr) }
				}

				// TODO sqlite3_normalized_sql (https://sqlite.org/c3ref/expanded_sql.html) // 3.27.0 + SQLITE_ENABLE_NORMALIZE
			}

			#[inline]
			pub(crate) unsafe fn expanded_sql(ptr: *mut ffi::sqlite3_stmt) -> Option<SqliteMallocString> {
				SqliteMallocString::from_raw(ffi::sqlite3_expanded_sql(ptr))
			}
			#[inline]
			pub(crate) unsafe fn stmt_status(
				ptr: *mut ffi::sqlite3_stmt,
				status: StatementStatus,
				reset: bool,
			) -> i32 {
				assert!(!ptr.is_null());
				ffi::sqlite3_stmt_status(ptr, status as i32, reset as i32)
			}

			impl Drop for RawStatement {
				fn drop(&mut self) {
					self.finalize_();
				}
			}

		}

		
        /// A prepared statement.
		pub struct Statement<'conn> {
			pub(crate) conn: &'conn Connection,
			pub(crate) stmt: RawStatement,
		}

		impl Statement<'_> {
			/// Execute the prepared statement.
			///
			/// On success, returns the number of rows that were changed or inserted or
			/// deleted (via `sqlite3_changes`).
			///
			/// ## Example
			///
			/// ### Use with positional parameters
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result, params};
			/// fn update_rows(conn: &Connection) -> Result<()> {
			///     let mut stmt = conn.prepare("UPDATE foo SET bar = ?1 WHERE qux = ?2")?;
			///     // For a single parameter, or a parameter where all the values have
			///     // the same type, just passing an array is simplest.
			///     stmt.execute([2i32])?;
			///     // The `rusqlite::params!` macro is mostly useful when the parameters do not
			///     // all have the same type, or if there are more than 32 parameters
			///     // at once, but it can be used in other cases.
			///     stmt.execute(params![1i32])?;
			///     // However, it's not required, many cases are fine as:
			///     stmt.execute(&[&2i32])?;
			///     // Or even:
			///     stmt.execute([2i32])?;
			///     // If you really want to, this is an option as well.
			///     stmt.execute((2i32,))?;
			///     Ok(())
			/// }
			/// ```
			///
			/// #### Heterogeneous positional parameters
			///
			/// ```
			/// use rusqlite::{Connection, Result};
			/// fn store_file(conn: &Connection, path: &str, data: &[u8]) -> Result<()> {
			///     # // no need to do it for real.
			///     # fn sha256(_: &[u8]) -> [u8; 32] { [0; 32] }
			///     let query = "INSERT OR REPLACE INTO files(path, hash, data) VALUES (?1, ?2, ?3)";
			///     let mut stmt = conn.prepare_cached(query)?;
			///     let hash: [u8; 32] = sha256(data);
			///     // The easiest way to pass positional parameters of have several
			///     // different types is by using a tuple.
			///     stmt.execute((path, hash, data))?;
			///     // Using the `params!` macro also works, and supports longer parameter lists:
			///     stmt.execute(rusqlite::params![path, hash, data])?;
			///     Ok(())
			/// }
			/// # let c = Connection::open_in_memory().unwrap();
			/// # c.execute_batch("CREATE TABLE files(path TEXT PRIMARY KEY, hash BLOB, data BLOB)").unwrap();
			/// # store_file(&c, "foo/bar.txt", b"bibble").unwrap();
			/// # store_file(&c, "foo/baz.txt", b"bobble").unwrap();
			/// ```
			///
			/// ### Use with named parameters
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result, named_params};
			/// fn insert(conn: &Connection) -> Result<()> {
			///     let mut stmt = conn.prepare("INSERT INTO test (key, value) VALUES (:key, :value)")?;
			///     // The `rusqlite::named_params!` macro (like `params!`) is useful for heterogeneous
			///     // sets of parameters (where all parameters are not the same type), or for queries
			///     // with many (more than 32) statically known parameters.
			///     stmt.execute(named_params! { ":key": "one", ":val": 2 })?;
			///     // However, named parameters can also be passed like:
			///     stmt.execute(&[(":key", "three"), (":val", "four")])?;
			///     // Or even: (note that a &T is required for the value type, currently)
			///     stmt.execute(&[(":key", &100), (":val", &200)])?;
			///     Ok(())
			/// }
			/// ```
			///
			/// ### Use without parameters
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result, params};
			/// fn delete_all(conn: &Connection) -> Result<()> {
			///     let mut stmt = conn.prepare("DELETE FROM users")?;
			///     stmt.execute([])?;
			///     Ok(())
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return `Err` if binding parameters fails, the executed statement
			/// returns rows (in which case `query` should be used instead), or the
			/// underlying SQLite call fails.
			#[inline] pub fn execute<P: Params>(&mut self, params: P) -> Result<usize> {
				params.__bind_in(self)?;
				self.execute_with_bound_parameters()
			}
			/// Execute an INSERT and return the ROWID.
			///
			/// # Note
			///
			/// This function is a convenience wrapper around
			/// [`execute()`](Statement::execute) intended for queries that insert a
			/// single item. It is possible to misuse this function in a way that it
			/// cannot detect, such as by calling it on a statement which _updates_
			/// a single item rather than inserting one. Please don't do that.
			///
			/// # Failure
			///
			/// Will return `Err` if no row is inserted or many rows are inserted.
			#[inline] pub fn insert<P: Params>(&mut self, params: P) -> Result<i64>
            {
				let changes = self.execute(params)?;
				match changes {
					1 => Ok(self.conn.last_insert_rowid()),
					_ => Err(Error::StatementChangedRows(changes)),
				}
			}
			/// Execute the prepared statement, returning a handle to the resulting
			/// rows.
			///
			/// Due to lifetime restrictions, the rows handle returned by `query` does
			/// not implement the `Iterator` trait. Consider using
			/// [`query_map`](Statement::query_map) or
			/// [`query_and_then`](Statement::query_and_then) instead, which do.
			///
			/// ## Example
			///
			/// ### Use without parameters
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn get_names(conn: &Connection) -> Result<Vec<String>> {
			///     let mut stmt = conn.prepare("SELECT name FROM people")?;
			///     let mut rows = stmt.query([])?;
			///
			///     let mut names = Vec::new();
			///     while let Some(row) = rows.next()? {
			///         names.push(row.get(0)?);
			///     }
			///
			///     Ok(names)
			/// }
			/// ```
			///
			/// ### Use with positional parameters
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn query(conn: &Connection, name: &str) -> Result<()> {
			///     let mut stmt = conn.prepare("SELECT * FROM test where name = ?1")?;
			///     let mut rows = stmt.query(rusqlite::params![name])?;
			///     while let Some(row) = rows.next()? {
			///         // ...
			///     }
			///     Ok(())
			/// }
			/// ```
			///
			/// Or, equivalently (but without the [`crate::params!`] macro).
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn query(conn: &Connection, name: &str) -> Result<()> {
			///     let mut stmt = conn.prepare("SELECT * FROM test where name = ?1")?;
			///     let mut rows = stmt.query([name])?;
			///     while let Some(row) = rows.next()? {
			///         // ...
			///     }
			///     Ok(())
			/// }
			/// ```
			///
			/// ### Use with named parameters
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn query(conn: &Connection) -> Result<()> {
			///     let mut stmt = conn.prepare("SELECT * FROM test where name = :name")?;
			///     let mut rows = stmt.query(&[(":name", "one")])?;
			///     while let Some(row) = rows.next()? {
			///         // ...
			///     }
			///     Ok(())
			/// }
			/// ```
			///
			/// Note, the `named_params!` macro is provided for syntactic convenience,
			/// and so the above example could also be written as:
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result, named_params};
			/// fn query(conn: &Connection) -> Result<()> {
			///     let mut stmt = conn.prepare("SELECT * FROM test where name = :name")?;
			///     let mut rows = stmt.query(named_params! { ":name": "one" })?;
			///     while let Some(row) = rows.next()? {
			///         // ...
			///     }
			///     Ok(())
			/// }
			/// ```
			///
			/// ## Failure
			///
			/// Will return `Err` if binding parameters fails.
			#[inline] pub fn query<P: Params>(&mut self, params: P) -> Result<Rows<'_>> {
				params.__bind_in(self)?;
				Ok(Rows::new(self))
			}
			/// Executes the prepared statement and maps a function over the resulting
			/// rows, returning an iterator over the mapped function results.
			///
			/// `f` is used to transform the _streaming_ iterator into a _standard_
			/// iterator.
			///
			/// This is equivalent to `stmt.query(params)?.mapped(f)`.
			///
			/// ## Example
			///
			/// ### Use with positional params
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn get_names(conn: &Connection) -> Result<Vec<String>> {
			///     let mut stmt = conn.prepare("SELECT name FROM people")?;
			///     let rows = stmt.query_map([], |row| row.get(0))?;
			///
			///     let mut names = Vec::new();
			///     for name_result in rows {
			///         names.push(name_result?);
			///     }
			///
			///     Ok(names)
			/// }
			/// ```
			///
			/// ### Use with named params
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn get_names(conn: &Connection) -> Result<Vec<String>> {
			///     let mut stmt = conn.prepare("SELECT name FROM people WHERE id = :id")?;
			///     let rows = stmt.query_map(&[(":id", &"one")], |row| row.get(0))?;
			///
			///     let mut names = Vec::new();
			///     for name_result in rows {
			///         names.push(name_result?);
			///     }
			///
			///     Ok(names)
			/// }
			/// ```
			/// ## Failure
			///
			/// Will return `Err` if binding parameters fails.
			pub fn query_map<T, P, F>(&mut self, params: P, f: F) -> Result<MappedRows<'_, F>>
			where
				P: Params,
				F: FnMut(&Row<'_>) -> Result<T>,
			{
				self.query(params).map(|rows| rows.mapped(f))
			}
			/// Executes the prepared statement and maps a function over the resulting
			/// rows, where the function returns a `Result` with `Error` type
			/// implementing `std::convert::From<Error>` (so errors can be unified).
			///
			/// This is equivalent to `stmt.query(params)?.and_then(f)`.
			///
			/// ## Example
			///
			/// ### Use with named params
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// struct Person {
			///     name: String,
			/// };
			///
			/// fn name_to_person(name: String) -> Result<Person> {
			///     // ... check for valid name
			///     Ok(Person { name })
			/// }
			///
			/// fn get_names(conn: &Connection) -> Result<Vec<Person>> {
			///     let mut stmt = conn.prepare("SELECT name FROM people WHERE id = :id")?;
			///     let rows = stmt.query_and_then(&[(":id", "one")], |row| name_to_person(row.get(0)?))?;
			///
			///     let mut persons = Vec::new();
			///     for person_result in rows {
			///         persons.push(person_result?);
			///     }
			///
			///     Ok(persons)
			/// }
			/// ```
			///
			/// ### Use with positional params
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn get_names(conn: &Connection) -> Result<Vec<String>> {
			///     let mut stmt = conn.prepare("SELECT name FROM people WHERE id = ?1")?;
			///     let rows = stmt.query_and_then(["one"], |row| row.get::<_, String>(0))?;
			///
			///     let mut persons = Vec::new();
			///     for person_result in rows {
			///         persons.push(person_result?);
			///     }
			///
			///     Ok(persons)
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return `Err` if binding parameters fails.
			#[inline] pub fn query_and_then<T, E, P, F>(&mut self, params: P, f: F) -> Result<AndThenRows<'_, F>>
			where
				P: Params,
				E: From<Error>,
				F: FnMut(&Row<'_>) -> Result<T, E>,
			{
				self.query(params).map(|rows| rows.and_then(f))
			}
			/// Return `true` if a query in the SQL statement it executes returns one
			/// or more rows and `false` if the SQL returns an empty set.
			#[inline] pub fn exists<P: Params>(&mut self, params: P) -> Result<bool>
            {
				let mut rows = self.query(params)?;
				let exists = rows.next()?.is_some();
				Ok(exists)
			}
			/// Convenience method to execute a query that is expected to return a
			/// single row.
			///
			/// If the query returns more than one row, all rows except the first are
			/// ignored.
			///
			/// Returns `Err(QueryReturnedNoRows)` if no results are returned. If the
			/// query truly is optional, you can call
			/// [`.optional()`](crate::OptionalExtension::optional) on the result of
			/// this to get a `Result<Option<T>>` (requires that the trait
			/// `rusqlite::OptionalExtension` is imported).
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			pub fn query_row<T, P, F>(&mut self, params: P, f: F) -> Result<T>
			where
				P: Params,
				F: FnOnce(&Row<'_>) -> Result<T>,
			{
				let mut rows = self.query(params)?;

				rows.get_expected_row().and_then(f)
			}
			/// Convenience method to execute a query that is expected to return exactly
			/// one row.
			///
			/// Returns `Err(QueryReturnedMoreThanOneRow)` if the query returns more than one row.
			///
			/// Returns `Err(QueryReturnedNoRows)` if no results are returned. If the
			/// query truly is optional, you can call
			/// [`.optional()`](crate::OptionalExtension::optional) on the result of
			/// this to get a `Result<Option<T>>` (requires that the trait
			/// `rusqlite::OptionalExtension` is imported).
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			pub fn query_one<T, P, F>(&mut self, params: P, f: F) -> Result<T>
			where
				P: Params,
				F: FnOnce(&Row<'_>) -> Result<T>,
			{
				let mut rows = self.query(params)?;
				let row = rows.get_expected_row().and_then(f)?;
				if rows.next()?.is_some() {
					return Err(Error::QueryReturnedMoreThanOneRow);
				}
				Ok(row)
			}
			/// Consumes the statement.
			///
			/// Functionally equivalent to the `Drop` implementation, but allows
			/// callers to see any errors that occur.
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			#[inline] pub fn finalize(mut self) -> Result<()> {
				self.finalize_()
			}
			/// Return the (one-based) index of an SQL parameter given its name.
			///
			/// Note that the initial ":" or "$" or "@" or "?" used to specify the
			/// parameter is included as part of the name.
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn example(conn: &Connection) -> Result<()> {
			///     let stmt = conn.prepare("SELECT * FROM test WHERE name = :example")?;
			///     let index = stmt.parameter_index(":example")?;
			///     assert_eq!(index, Some(1));
			///     Ok(())
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return Err if `name` is invalid. Will return Ok(None) if the name
			/// is valid but not a bound parameter of this statement.
			#[inline] pub fn parameter_index(&self, name: &str) -> Result<Option<usize>> {
				Ok(self.stmt.bind_parameter_index(name))
			}
			/// Return the SQL parameter name given its (one-based) index (the inverse
			/// of [`Statement::parameter_index`]).
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn example(conn: &Connection) -> Result<()> {
			///     let stmt = conn.prepare("SELECT * FROM test WHERE name = :example")?;
			///     let index = stmt.parameter_name(1);
			///     assert_eq!(index, Some(":example"));
			///     Ok(())
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return `None` if the column index is out of bounds or if the
			/// parameter is positional.
			///
			/// # Panics
			///
			/// Panics when parameter name is not valid UTF-8.
			#[inline] pub fn parameter_name(&self, index: usize) -> Option<&'_ str> {
				self.stmt.bind_parameter_name(index as i32).map(|name| {
					name.to_str()
						.expect("Invalid UTF-8 sequence in parameter name")
				})
			}

			#[inline]
			pub(crate) fn bind_parameters<P>(&mut self, params: P) -> Result<()>
			where
				P: IntoIterator,
				P::Item: ToSql,
			{
				let expected = self.stmt.bind_parameter_count();
				let mut index = 0;
				for p in params {
					index += 1; // The leftmost SQL parameter has an index of 1.
					if index > expected {
						break;
					}
					self.bind_parameter(&p, index)?;
				}
				if index != expected {
					Err(Error::InvalidParameterCount(index, expected))
				} else {
					Ok(())
				}
			}

			#[inline]
			pub(crate) fn ensure_parameter_count(&self, n: usize) -> Result<()>
            {
				let count = self.parameter_count();
				if count != n {
					Err(Error::InvalidParameterCount(n, count))
				} else {
					Ok(())
				}
			}

			#[inline]
			pub(crate) fn bind_parameters_named<S: BindIndex, T: ToSql>(
				&mut self,
				params: &[(S, T)],
			) -> Result<()> {
				for (name, value) in params {
					let i = name.idx(self)?;
					let ts: &dyn ToSql = &value;
					self.bind_parameter(ts, i)?;
				}
				Ok(())
			}
			/// Return the number of parameters that can be bound to this statement.
			#[inline] pub fn parameter_count(&self) -> usize {
				self.stmt.bind_parameter_count()
			}
			/// Low level API to directly bind a parameter to a given index.
			///
			/// Note that the index is one-based, that is, the first parameter index is
			/// 1 and not 0. This is consistent with the SQLite API and the values given
			/// to parameters bound as `?NNN`.
			///
			/// The valid values for `one_based_col_index` begin at `1`, and end at
			/// [`Statement::parameter_count`], inclusive.
			///
			/// # Caveats
			///
			/// This should not generally be used, but is available for special cases
			/// such as:
			///
			/// - binding parameters where a gap exists.
			/// - binding named and positional parameters in the same query.
			/// - separating parameter binding from query execution.
			///
			/// In general, statements that have had *any* parameters bound this way
			/// should have *all* parameters bound this way, and be queried or executed
			/// by [`Statement::raw_query`] or [`Statement::raw_execute`], other usage
			/// is unsupported and will likely, probably in surprising ways.
			///
			/// That is: Do not mix the "raw" statement functions with the rest of the
			/// API, or the results may be surprising, and may even change in future
			/// versions without comment.
			///
			/// # Example
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// fn query(conn: &Connection) -> Result<()> {
			///     let mut stmt = conn.prepare("SELECT * FROM test WHERE name = :name AND value > ?2")?;
			///     stmt.raw_bind_parameter(c":name", "foo")?;
			///     stmt.raw_bind_parameter(2, 100)?;
			///     let mut rows = stmt.raw_query();
			///     while let Some(row) = rows.next()? {
			///         // ...
			///     }
			///     Ok(())
			/// }
			/// ```
			#[inline] pub fn raw_bind_parameter<I: BindIndex, T: ToSql>(
				&mut self,
				one_based_index: I,
				param: T,
			) -> Result<()> {
				// This is the same as `bind_parameter` but slightly more ergonomic and
				// correctly takes `&mut self`.
				self.bind_parameter(&param, one_based_index.idx(self)?)
			}
			/// Low level API to execute a statement given that all parameters were
			/// bound explicitly with the [`Statement::raw_bind_parameter`] API.
			///
			/// # Caveats
			///
			/// Any unbound parameters will have `NULL` as their value.
			///
			/// This should not generally be used outside special cases, and
			/// functions in the [`Statement::execute`] family should be preferred.
			///
			/// # Failure
			///
			/// Will return `Err` if the executed statement returns rows (in which case
			/// `query` should be used instead), or the underlying SQLite call fails.
			#[inline] pub fn raw_execute(&mut self) -> Result<usize> {
				self.execute_with_bound_parameters()
			}
			/// Low level API to get `Rows` for this query given that all parameters
			/// were bound explicitly with the [`Statement::raw_bind_parameter`] API.
			///
			/// # Caveats
			///
			/// Any unbound parameters will have `NULL` as their value.
			///
			/// This should not generally be used outside special cases, and
			/// functions in the [`Statement::query`] family should be preferred.
			///
			/// Note that if the SQL does not return results, [`Statement::raw_execute`]
			/// should be used instead.
			#[inline] pub fn raw_query(&mut self) -> Rows<'_> {
				Rows::new(self)
			}

			// generic because many of these branches can constant fold away.
			fn bind_parameter<P: ?Sized + ToSql>(&self, param: &P, ndx: usize) -> Result<()>
            {
				let value = param.to_sql()?;

				let ptr = unsafe { self.stmt.ptr() };
				let value = match value {
					ToSqlOutput::Borrowed(v) => v,
					ToSqlOutput::Owned(ref v) => ValueRef::from(v),

					#[cfg(feature = "blob")]
					ToSqlOutput::ZeroBlob(len) => {
						// TODO sqlite3_bind_zeroblob64 // 3.8.11
						return self
							.conn
							.decode_result(unsafe { ffi::sqlite3_bind_zeroblob(ptr, ndx as c_int, len) });
					}
					#[cfg(feature = "functions")]
					ToSqlOutput::Arg(_) => {
						return Err(err!(ffi::SQLITE_MISUSE, "Unsupported value \"{value:?}\""));
					}
					#[cfg(feature = "array")]
					ToSqlOutput::Array(a) => {
						return self.conn.decode_result(unsafe {
							ffi::sqlite3_bind_pointer(
								ptr,
								ndx as c_int,
								Rc::into_raw(a) as *mut c_void,
								ARRAY_TYPE,
								Some(free_array),
							)
						});
					}
				};
				self.conn.decode_result(match value {
					ValueRef::Null => unsafe { ffi::sqlite3_bind_null(ptr, ndx as c_int) },
					ValueRef::Integer(i) => unsafe { ffi::sqlite3_bind_int64(ptr, ndx as c_int, i) },
					ValueRef::Real(r) => unsafe { ffi::sqlite3_bind_double(ptr, ndx as c_int, r) },
					ValueRef::Text(s) => unsafe {
						let (c_str, len, destructor) = str_for_sqlite(s)?;
						// TODO sqlite3_bind_text64 // 3.8.7
						ffi::sqlite3_bind_text(ptr, ndx as c_int, c_str, len, destructor)
					},
					ValueRef::Blob(b) => unsafe {
						let length = len_as_c_int(b.len())?;
						if length == 0 {
							ffi::sqlite3_bind_zeroblob(ptr, ndx as c_int, 0)
						} else {
							// TODO sqlite3_bind_blob64 // 3.8.7
							ffi::sqlite3_bind_blob(
								ptr,
								ndx as c_int,
								b.as_ptr().cast::<c_void>(),
								length,
								ffi::SQLITE_TRANSIENT(),
							)
						}
					},
				})
			}

			#[inline] fn execute_with_bound_parameters(&mut self) -> Result<usize> {
				self.check_update()?;
				let r = self.stmt.step();
				let rr = self.stmt.reset();
				match r {
					ffi::SQLITE_DONE => match rr {
						ffi::SQLITE_OK => Ok(self.conn.changes() as usize),
						_ => Err(self.conn.decode_result(rr).unwrap_err()),
					},
					ffi::SQLITE_ROW => Err(Error::ExecuteReturnedResults),
					_ => Err(self.conn.decode_result(r).unwrap_err()),
				}
			}

			#[inline] fn finalize_(&mut self) -> Result<()>
            {
				let mut stmt = unsafe { RawStatement::new(ptr::null_mut()) };
				mem::swap(&mut stmt, &mut self.stmt);
				self.conn.decode_result(stmt.finalize())
			}

			#[cfg(feature = "extra_check")]
			#[inline] fn check_update(&self) -> Result<()> {
				if self.column_count() > 0 && self.stmt.readonly() {
					return Err(Error::ExecuteReturnedResults);
				}
				Ok(())
			}

			#[cfg(not(feature = "extra_check"))]
			#[inline]
			#[expect(clippy::unnecessary_wraps)]
			fn check_update(&self) -> Result<()> {
				Ok(())
			}
			/// Returns a string containing the SQL text of prepared statement with
			/// bound parameters expanded.
			pub fn expanded_sql(&self) -> Option<String> {
				self.stmt
					.expanded_sql()
					.map(|s| s.to_string_lossy().to_string())
			}
			/// Get the value for one of the status counters for this statement.
			#[inline] pub fn get_status(&self, status: StatementStatus) -> i32 {
				self.stmt.get_status(status, false)
			}
			/// Reset the value of one of the status counters for this statement,
			#[inline]
			/// returning the value it had before resetting.
			pub fn reset_status(&self, status: StatementStatus) -> i32 {
				self.stmt.get_status(status, true)
			}
			/// Returns 1 if the prepared statement is an EXPLAIN statement,
			/// or 2 if the statement is an EXPLAIN QUERY PLAN,
			/// or 0 if it is an ordinary statement or a NULL pointer.
			#[inline]
			#[cfg(feature = "modern_sqlite")] // 3.28.0
			pub fn is_explain(&self) -> i32 {
				self.stmt.is_explain()
			}
			/// Returns true if the statement is read only.
			#[inline] pub fn readonly(&self) -> bool {
				self.stmt.readonly()
			}
			/// Safety: This is unsafe, because using `sqlite3_stmt` after the
			/// connection has closed is illegal, but `RawStatement` does not enforce
			/// this, as it loses our protective `'conn` lifetime bound.
			#[inline]
			pub(crate) unsafe fn into_raw(mut self) -> RawStatement {
				let mut stmt = RawStatement::new(ptr::null_mut());
				mem::swap(&mut stmt, &mut self.stmt);
				stmt
			}
			/// Reset all bindings
			pub fn clear_bindings(&mut self) {
				self.stmt.clear_bindings();
			}

			pub(crate) unsafe fn ptr(&self) -> *mut ffi::sqlite3_stmt {
				self.stmt.ptr()
			}
		}

		impl fmt::Debug for Statement<'_> {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				let sql = if self.stmt.is_null() {
					Ok("")
				} else {
					self.stmt.sql().unwrap().to_str()
				};
				f.debug_struct("Statement")
					.field("conn", self.conn)
					.field("stmt", &self.stmt)
					.field("sql", &sql)
					.finish()
			}
		}

		impl Drop for Statement<'_> {
			#[expect(unused_must_use)]
			#[inline] fn drop(&mut self) {
				self.finalize_();
			}
		}

		impl Statement<'_> {
			#[inline]
			pub(super) fn new(conn: &Connection, stmt: RawStatement) -> Statement<'_> {
				Statement { conn, stmt }
			}

			pub(super) fn value_ref(&self, col: usize) -> ValueRef<'_>
            {
				let raw = unsafe { self.stmt.ptr() };

				match self.stmt.column_type(col) {
					ffi::SQLITE_NULL => ValueRef::Null,
					ffi::SQLITE_INTEGER => {
						ValueRef::Integer(unsafe { ffi::sqlite3_column_int64(raw, col as c_int) })
					}
					ffi::SQLITE_FLOAT => {
						ValueRef::Real(unsafe { ffi::sqlite3_column_double(raw, col as c_int) })
					}
					ffi::SQLITE_TEXT => {
						let s = unsafe {
							// Quoting from "Using SQLite" book:
							// To avoid problems, an application should first extract the desired type using
							// a sqlite3_column_xxx() function, and then call the
							// appropriate sqlite3_column_bytes() function.
							let text = ffi::sqlite3_column_text(raw, col as c_int);
							let len = ffi::sqlite3_column_bytes(raw, col as c_int);
							assert!(
								!text.is_null(),
								"unexpected SQLITE_TEXT column type with NULL data"
							);
							from_raw_parts(text.cast::<u8>(), len as usize)
						};

						ValueRef::Text(s)
					}
					ffi::SQLITE_BLOB => {
						let (blob, len) = unsafe {
							(
								ffi::sqlite3_column_blob(raw, col as c_int),
								ffi::sqlite3_column_bytes(raw, col as c_int),
							)
						};

						assert!(
							len >= 0,
							"unexpected negative return from sqlite3_column_bytes"
						);
						if len > 0 {
							assert!(
								!blob.is_null(),
								"unexpected SQLITE_BLOB column type with NULL data"
							);
							ValueRef::Blob(unsafe { from_raw_parts(blob.cast::<u8>(), len as usize) })
						} else {
							// The return value from sqlite3_column_blob() for a zero-length BLOB
							// is a NULL pointer.
							ValueRef::Blob(&[])
						}
					}
					_ => unreachable!("sqlite3_column_type returned invalid value"),
				}
			}

			#[inline]
			pub(super) fn step(&self) -> Result<bool> {
				match self.stmt.step() {
					ffi::SQLITE_ROW => Ok(true),
					ffi::SQLITE_DONE => Ok(false),
					code => Err(self.conn.decode_result(code).unwrap_err()),
				}
			}

			#[inline]
			pub(super) fn reset(&self) -> Result<()> {
				match self.stmt.reset() {
					ffi::SQLITE_OK => Ok(()),
					code => Err(self.conn.decode_result(code).unwrap_err()),
				}
			}
		}

		/// Prepared statement status counters.
		///
		/// See `https://www.sqlite.org/c3ref/c_stmtstatus_counter.html`
		/// for explanations of each.
		///
		/// Note that depending on your version of SQLite, all of these
		/// may not be available.
		#[repr(i32)]
		#[derive(Clone, Copy, PartialEq, Eq)]
		#[non_exhaustive]
		pub enum StatementStatus {
			/// Equivalent to `SQLITE_STMTSTATUS_FULLSCAN_STEP`
			FullscanStep = 1,
			/// Equivalent to `SQLITE_STMTSTATUS_SORT`
			Sort = 2,
			/// Equivalent to `SQLITE_STMTSTATUS_AUTOINDEX`
			AutoIndex = 3,
			/// Equivalent to `SQLITE_STMTSTATUS_VM_STEP`
			VmStep = 4,
			/// Equivalent to `SQLITE_STMTSTATUS_REPREPARE` (3.20.0)
			RePrepare = 5,
			/// Equivalent to `SQLITE_STMTSTATUS_RUN` (3.20.0)
			Run = 6,
			/// Equivalent to `SQLITE_STMTSTATUS_FILTER_MISS`
			FilterMiss = 7,
			/// Equivalent to `SQLITE_STMTSTATUS_FILTER_HIT`
			FilterHit = 8,
			/// Equivalent to `SQLITE_STMTSTATUS_MEMUSED` (3.20.0)
			MemUsed = 99,
		}
    } pub use self::statement::{ Statement, StatementStatus, raw::RawStatement };

	pub mod transaction
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use crate::{Connection, Result};
		use std::ops::Deref;
        */
		/// Options for transaction behavior. See [BEGIN
		/// TRANSACTION](http://www.sqlite.org/lang_transaction.html) for details.
		#[derive(Copy, Clone)]
		#[non_exhaustive]
		pub enum TransactionBehavior {
			/// DEFERRED means that the transaction does not actually start until the
			/// database is first accessed.
			Deferred,
			/// IMMEDIATE cause the database connection to start a new write
			/// immediately, without waiting for a writes statement.
			Immediate,
			/// EXCLUSIVE prevents other database connections from reading the database
			/// while the transaction is underway.
			Exclusive,
		}

		/// Options for how a Transaction or Savepoint should behave when it is dropped.
		#[derive(Copy, Clone, Debug, PartialEq, Eq)]
		#[non_exhaustive]
		pub enum DropBehavior {
			/// Roll back the changes. This is the default.
			Rollback,

			/// Commit the changes.
			Commit,

			/// Do not commit or roll back changes - this will leave the transaction or
			/// savepoint open, so should be used with care.
			Ignore,

			/// Panic. Used to enforce intentional behavior during development.
			Panic,
		}

		/// Represents a transaction on a database connection.
		///
		/// ## Note
		///
		/// Transactions will roll back by default. Use `commit` method to explicitly
		/// commit the transaction, or use `set_drop_behavior` to change what happens
		/// when the transaction is dropped.
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// # fn do_queries_part_1(_conn: &Connection) -> Result<()> { Ok(()) }
		/// # fn do_queries_part_2(_conn: &Connection) -> Result<()> { Ok(()) }
		/// fn perform_queries(conn: &mut Connection) -> Result<()> {
		///     let tx = conn.transaction()?;
		///
		///     do_queries_part_1(&tx)?; // tx causes rollback if this fails
		///     do_queries_part_2(&tx)?; // tx causes rollback if this fails
		///
		///     tx.commit()
		/// }
		/// ```
		#[derive(Debug)]
		pub struct Transaction<'conn> {
			conn: &'conn Connection,
			drop_behavior: DropBehavior,
		}

		/// Represents a savepoint on a database connection.
		///
		/// ## Note
		///
		/// Savepoints will roll back by default. Use `commit` method to explicitly
		/// commit the savepoint, or use `set_drop_behavior` to change what happens
		/// when the savepoint is dropped.
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// # fn do_queries_part_1(_conn: &Connection) -> Result<()> { Ok(()) }
		/// # fn do_queries_part_2(_conn: &Connection) -> Result<()> { Ok(()) }
		/// fn perform_queries(conn: &mut Connection) -> Result<()> {
		///     let sp = conn.savepoint()?;
		///
		///     do_queries_part_1(&sp)?; // sp causes rollback if this fails
		///     do_queries_part_2(&sp)?; // sp causes rollback if this fails
		///
		///     sp.commit()
		/// }
		/// ```
		#[derive(Debug)]
		pub struct Savepoint<'conn> {
			conn: &'conn Connection,
			name: String,
			drop_behavior: DropBehavior,
			committed: bool,
		}

		impl Transaction<'_> {
			/// Begin a new transaction. Cannot be nested; see `savepoint` for nested
			/// transactions.
			///
			/// Even though we don't mutate the connection, we take a `&mut Connection`
			/// to prevent nested transactions on the same connection. For cases
			/// where this is unacceptable, [`Transaction::new_unchecked`] is available.
			#[inline] pub fn new(conn: &mut Connection, behavior: TransactionBehavior) -> Result<Transaction<'_>> {
				Self::new_unchecked(conn, behavior)
			}
			/// Begin a new transaction, failing if a transaction is open.
			///
			/// If a transaction is already open, this will return an error. Where
			/// possible, [`Transaction::new`] should be preferred, as it provides a
			/// compile-time guarantee that transactions are not nested.
			#[inline] pub fn new_unchecked(
				conn: &Connection,
				behavior: TransactionBehavior,
			) -> Result<Transaction<'_>>
            {
				let query = match behavior {
					TransactionBehavior::Deferred => "BEGIN DEFERRED",
					TransactionBehavior::Immediate => "BEGIN IMMEDIATE",
					TransactionBehavior::Exclusive => "BEGIN EXCLUSIVE",
				};
				conn.execute_batch(query).map(move |()| Transaction {
					conn,
					drop_behavior: DropBehavior::Rollback,
				})
			}
			/// Starts a new [savepoint](http://www.sqlite.org/lang_savepoint.html), allowing nested
			/// transactions.
			///
			/// ## Note
			///
			/// Just like outer level transactions, savepoint transactions rollback by
			/// default.
			///
			/// ## Example
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// # fn perform_queries_part_1_succeeds(_conn: &Connection) -> bool { true }
			/// fn perform_queries(conn: &mut Connection) -> Result<()> {
			///     let mut tx = conn.transaction()?;
			///
			///     {
			///         let sp = tx.savepoint()?;
			///         if perform_queries_part_1_succeeds(&sp) {
			///             sp.commit()?;
			///         }
			///         // otherwise, sp will rollback
			///     }
			///
			///     tx.commit()
			/// }
			/// ```
			#[inline] pub fn savepoint(&mut self) -> Result<Savepoint<'_>> {
				Savepoint::new_(self.conn)
			}
			/// Create a new savepoint with a custom savepoint name. See `savepoint()`.
			#[inline] pub fn savepoint_with_name<T: Into<String>>(&mut self, name: T) -> Result<Savepoint<'_>> {
				Savepoint::with_name_(self.conn, name)
			}
			/// Get the current setting for what happens to the transaction when it is
			/// dropped.
			#[inline]
			#[must_use]
			pub fn drop_behavior(&self) -> DropBehavior {
				self.drop_behavior
			}
			/// Configure the transaction to perform the specified action when it is
			/// dropped.
			#[inline] pub fn set_drop_behavior(&mut self, drop_behavior: DropBehavior) {
				self.drop_behavior = drop_behavior;
			}
			/// A convenience method which consumes and commits a transaction.
			#[inline] pub fn commit(mut self) -> Result<()> {
				self.commit_()
			}

			#[inline] fn commit_(&mut self) -> Result<()> {
				self.conn.execute_batch("COMMIT")?;
				Ok(())
			}
			/// A convenience method which consumes and rolls back a transaction.
			#[inline] pub fn rollback(mut self) -> Result<()> {
				self.rollback_()
			}

			#[inline] fn rollback_(&mut self) -> Result<()> {
				self.conn.execute_batch("ROLLBACK")?;
				Ok(())
			}
			/// Consumes the transaction, committing or rolling back according to the
			/// current setting (see `drop_behavior`).
			///
			/// Functionally equivalent to the `Drop` implementation, but allows
			/// callers to see any errors that occur.
			#[inline] pub fn finish(mut self) -> Result<()> {
				self.finish_()
			}

			#[inline] fn finish_(&mut self) -> Result<()> {
				if self.conn.is_autocommit() {
					return Ok(());
				}
				match self.drop_behavior() {
					DropBehavior::Commit => self.commit_().or_else(|_| self.rollback_()),
					DropBehavior::Rollback => self.rollback_(),
					DropBehavior::Ignore => Ok(()),
					DropBehavior::Panic => panic!("Transaction dropped unexpectedly."),
				}
			}
		}

		impl Deref for Transaction<'_> {
			type Target = Connection;

			#[inline] fn deref(&self) -> &Connection {
				self.conn
			}
		}

		#[expect(unused_must_use)]
		impl Drop for Transaction<'_> {
			#[inline] fn drop(&mut self) {
				self.finish_();
			}
		}

		impl Savepoint<'_> {
			#[inline] fn with_name_<T: Into<String>>(conn: &Connection, name: T) -> Result<Savepoint<'_>>
            {
				let name = name.into();
				conn.execute_batch(&format!("SAVEPOINT {name}"))
					.map(|()| Savepoint {
						conn,
						name,
						drop_behavior: DropBehavior::Rollback,
						committed: false,
					})
			}

			#[inline] fn new_(conn: &Connection) -> Result<Savepoint<'_>> {
				Savepoint::with_name_(conn, "_rusqlite_sp")
			}
			/// Begin a new savepoint. Can be nested.
			#[inline] pub fn new(conn: &mut Connection) -> Result<Savepoint<'_>> {
				Savepoint::new_(conn)
			}
			/// Begin a new savepoint with a user-provided savepoint name.
			#[inline] pub fn with_name<T: Into<String>>(conn: &mut Connection, name: T) -> Result<Savepoint<'_>> {
				Savepoint::with_name_(conn, name)
			}
			/// Begin a nested savepoint.
			#[inline] pub fn savepoint(&mut self) -> Result<Savepoint<'_>> {
				Savepoint::new_(self.conn)
			}
			/// Begin a nested savepoint with a user-provided savepoint name.
			#[inline] pub fn savepoint_with_name<T: Into<String>>(&mut self, name: T) -> Result<Savepoint<'_>> {
				Savepoint::with_name_(self.conn, name)
			}
			/// Get the current setting for what happens to the savepoint when it is
			/// dropped.
			#[inline]
			#[must_use]
			pub fn drop_behavior(&self) -> DropBehavior {
				self.drop_behavior
			}
			/// Configure the savepoint to perform the specified action when it is
			/// dropped.
			#[inline] pub fn set_drop_behavior(&mut self, drop_behavior: DropBehavior) {
				self.drop_behavior = drop_behavior;
			}
			/// A convenience method which consumes and commits a savepoint.
			#[inline] pub fn commit(mut self) -> Result<()> {
				self.commit_()
			}

			#[inline] fn commit_(&mut self) -> Result<()> {
				self.conn.execute_batch(&format!("RELEASE {}", self.name))?;
				self.committed = true;
				Ok(())
			}
			/// A convenience method which rolls back a savepoint.
			///
			/// ## Note
			///
			/// Unlike `Transaction`s, savepoints remain active after they have been
			/// rolled back, and can be rolled back again or committed.
			#[inline] pub fn rollback(&mut self) -> Result<()> {
				self.conn
					.execute_batch(&format!("ROLLBACK TO {}", self.name))
			}
			/// Consumes the savepoint, committing or rolling back according to the
			/// current setting (see `drop_behavior`).
			///
			/// Functionally equivalent to the `Drop` implementation, but allows
			/// callers to see any errors that occur.
			#[inline] pub fn finish(mut self) -> Result<()> {
				self.finish_()
			}

			#[inline] fn finish_(&mut self) -> Result<()> {
				if self.committed {
					return Ok(());
				}
				match self.drop_behavior() {
					DropBehavior::Commit => self
						.commit_()
						.or_else(|_| self.rollback().and_then(|()| self.commit_())),
					DropBehavior::Rollback => self.rollback().and_then(|()| self.commit_()),
					DropBehavior::Ignore => Ok(()),
					DropBehavior::Panic => panic!("Savepoint dropped unexpectedly."),
				}
			}
		}

		impl Deref for Savepoint<'_> {
			type Target = Connection;

			#[inline] fn deref(&self) -> &Connection {
				self.conn
			}
		}

		#[expect(unused_must_use)]
		impl Drop for Savepoint<'_> {
			#[inline] fn drop(&mut self) {
				self.finish_();
			}
		}

		/// Transaction state of a database
		#[derive(Clone, Copy, Debug, PartialEq, Eq)]
		#[non_exhaustive]
		#[cfg(feature = "modern_sqlite")] // 3.37.0
		pub enum TransactionState {
			/// Equivalent to `SQLITE_TXN_NONE`
			None,
			/// Equivalent to `SQLITE_TXN_READ`
			Read,
			/// Equivalent to `SQLITE_TXN_WRITE`
			Write,
		}

		impl Connection {
			/// Begin a new transaction with the default behavior (DEFERRED).
			///
			/// The transaction defaults to rolling back when it is dropped. If you
			/// want the transaction to commit, you must call
			/// [`commit`](Transaction::commit) or
			/// [`set_drop_behavior(DropBehavior::Commit)`](Transaction::set_drop_behavior).
			///
			/// ## Example
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// # fn do_queries_part_1(_conn: &Connection) -> Result<()> { Ok(()) }
			/// # fn do_queries_part_2(_conn: &Connection) -> Result<()> { Ok(()) }
			/// fn perform_queries(conn: &mut Connection) -> Result<()> {
			///     let tx = conn.transaction()?;
			///
			///     do_queries_part_1(&tx)?; // tx causes rollback if this fails
			///     do_queries_part_2(&tx)?; // tx causes rollback if this fails
			///
			///     tx.commit()
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			#[inline] pub fn transaction(&mut self) -> Result<Transaction<'_>> {
				Transaction::new(self, self.transaction_behavior)
			}
			/// Begin a new transaction with a specified behavior.
			///
			/// See [`transaction`](Connection::transaction).
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			#[inline] pub fn transaction_with_behavior(
				&mut self,
				behavior: TransactionBehavior,
			) -> Result<Transaction<'_>> {
				Transaction::new(self, behavior)
			}
			/// Begin a new transaction with the default behavior (DEFERRED).
			///
			/// Attempt to open a nested transaction will result in a SQLite error.
			/// `Connection::transaction` prevents this at compile time by taking `&mut
			/// self`, but `Connection::unchecked_transaction()` may be used to defer
			/// the checking until runtime.
			///
			/// See [`Connection::transaction`] and [`Transaction::new_unchecked`]
			/// (which can be used if the default transaction behavior is undesirable).
			///
			/// ## Example
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// # use std::rc::Rc;
			/// # fn do_queries_part_1(_conn: &Connection) -> Result<()> { Ok(()) }
			/// # fn do_queries_part_2(_conn: &Connection) -> Result<()> { Ok(()) }
			/// fn perform_queries(conn: Rc<Connection>) -> Result<()> {
			///     let tx = conn.unchecked_transaction()?;
			///
			///     do_queries_part_1(&tx)?; // tx causes rollback if this fails
			///     do_queries_part_2(&tx)?; // tx causes rollback if this fails
			///
			///     tx.commit()
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails. The specific
			/// error returned if transactions are nested is currently unspecified.
			pub fn unchecked_transaction(&self) -> Result<Transaction<'_>> {
				Transaction::new_unchecked(self, self.transaction_behavior)
			}
			/// Begin a new savepoint with the default behavior (DEFERRED).
			///
			/// The savepoint defaults to rolling back when it is dropped. If you want
			/// the savepoint to commit, you must call [`commit`](Savepoint::commit) or
			/// [`set_drop_behavior(DropBehavior::Commit)`](Savepoint::set_drop_behavior).
			///
			/// ## Example
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result};
			/// # fn do_queries_part_1(_conn: &Connection) -> Result<()> { Ok(()) }
			/// # fn do_queries_part_2(_conn: &Connection) -> Result<()> { Ok(()) }
			/// fn perform_queries(conn: &mut Connection) -> Result<()> {
			///     let sp = conn.savepoint()?;
			///
			///     do_queries_part_1(&sp)?; // sp causes rollback if this fails
			///     do_queries_part_2(&sp)?; // sp causes rollback if this fails
			///
			///     sp.commit()
			/// }
			/// ```
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			#[inline] pub fn savepoint(&mut self) -> Result<Savepoint<'_>> {
				Savepoint::new(self)
			}
			/// Begin a new savepoint with a specified name.
			///
			/// See [`savepoint`](Connection::savepoint).
			///
			/// # Failure
			///
			/// Will return `Err` if the underlying SQLite call fails.
			#[inline] pub fn savepoint_with_name<T: Into<String>>(&mut self, name: T) -> Result<Savepoint<'_>> {
				Savepoint::with_name(self, name)
			}
			/// Determine the transaction state of a database
			#[cfg(feature = "modern_sqlite")] // 3.37.0
			pub fn transaction_state<N: crate::Name>(
				&self,
				db_name: Option<N>,
			) -> Result<TransactionState> {
				self.db.borrow().txn_state(db_name)
			}
			/// Set the default transaction behavior for the connection.
			///
			/// ## Note
			///
			/// This will only apply to transactions initiated by [`transaction`](Connection::transaction)
			/// or [`unchecked_transaction`](Connection::unchecked_transaction).
			///
			/// ## Example
			///
			/// ```rust,no_run
			/// # use rusqlite::{Connection, Result, TransactionBehavior};
			/// # fn do_queries_part_1(_conn: &Connection) -> Result<()> { Ok(()) }
			/// # fn do_queries_part_2(_conn: &Connection) -> Result<()> { Ok(()) }
			/// fn perform_queries(conn: &mut Connection) -> Result<()> {
			///     conn.set_transaction_behavior(TransactionBehavior::Immediate);
			///
			///     let tx = conn.transaction()?;
			///
			///     do_queries_part_1(&tx)?; // tx causes rollback if this fails
			///     do_queries_part_2(&tx)?; // tx causes rollback if this fails
			///
			///     tx.commit()
			/// }
			/// ```
			pub fn set_transaction_behavior(&mut self, behavior: TransactionBehavior) {
				self.transaction_behavior = behavior;
			}
		}
    } pub use self::transaction::{ DropBehavior, Savepoint, Transaction, TransactionBehavior, TransactionState };

	pub mod types
    {
        /*!
        Traits dealing with SQLite data types. */
        use ::
        {
            *,
        };
        /*
		pub use self::to_sql::{ToSql, ToSqlOutput};
		pub use self::value::Value;
		pub use self::value_ref::ValueRef;

		use std::fmt;
		*/
		mod from_sql
		{
			/*!
			*/
			use ::
			{
                borrow::{ Cow },
                error::{ Error },
                sqlite3::
                {
                    types::{ Value, ValueRef },
                },

				*,
			};
			/*
			*/
			/// Enum listing possible errors from [`FromSql`] trait.
			#[derive(Debug)]
			#[non_exhaustive]
			pub enum FromSqlError {
				/// Error when an SQLite value is requested, but the type of the result
				/// cannot be converted to the requested Rust type.
				InvalidType,

				/// Error when the i64 value returned by SQLite cannot be stored into the
				/// requested type.
				OutOfRange(i64),

				/// Error when the blob result returned by SQLite cannot be stored into the
				/// requested type due to a size mismatch.
				InvalidBlobSize {
					/// The expected size of the blob.
					expected_size: usize,
					/// The actual size of the blob that was returned.
					blob_size: usize,
				},

				/// An error case available for implementors of the [`FromSql`] trait.
				Other(Box<dyn Error + Send + Sync + 'static>),
			}

			impl FromSqlError {
				/// Converts an arbitrary error type to [`FromSqlError`].
				///
				/// This is a convenience function that boxes and unsizes the error type. It's main purpose is
				/// to be usable in the `map_err` method. So instead of
				/// `result.map_err(|error| FromSqlError::Other(Box::new(error))` you can write
				/// `result.map_err(FromSqlError::other)`.
				pub fn other<E: Error + Send + Sync + 'static>(error: E) -> Self {
					Self::Other(Box::new(error))
				}
			}

			impl PartialEq for FromSqlError {
				fn eq(&self, other: &Self) -> bool {
					match (self, other) {
						(Self::InvalidType, Self::InvalidType) => true,
						(Self::OutOfRange(n1), Self::OutOfRange(n2)) => n1 == n2,
						(
							Self::InvalidBlobSize {
								expected_size: es1,
								blob_size: bs1,
							},
							Self::InvalidBlobSize {
								expected_size: es2,
								blob_size: bs2,
							},
						) => es1 == es2 && bs1 == bs2,
						(..) => false,
					}
				}
			}

			impl fmt::Display for FromSqlError {
				fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
					match *self {
						Self::InvalidType => write!(f, "Invalid type"),
						Self::OutOfRange(i) => write!(f, "Value {i} out of range"),
						Self::InvalidBlobSize {
							expected_size,
							blob_size,
						} => {
							write!(
								f,
								"Cannot read {expected_size} byte value out of {blob_size} byte blob"
							)
						}
						Self::Other(ref err) => err.fmt(f),
					}
				}
			}

			impl Error for FromSqlError {
				fn source(&self) -> Option<&(dyn Error + 'static)> {
					if let Self::Other(ref err) = self {
						Some(&**err)
					} else {
						None
					}
				}
			}
			/// Result type for implementors of the [`FromSql`] trait.
			pub type FromSqlResult<T> = Result<T, FromSqlError>;

			/// A trait for types that can be created from a SQLite value.
			pub trait FromSql: Sized {
				/// Converts SQLite value into Rust value.
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self>;
			}

			macro_rules! from_sql_integral(
				($t:ident) => (
					impl FromSql for $t {
						#[inline]
						fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
							let i = i64::column_result(value)?;
							i.try_into().map_err(|_| FromSqlError::OutOfRange(i))
						}
					}
				);
				(non_zero $nz:ty, $z:ty) => (
					impl FromSql for $nz {
						#[inline]
						fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
							let i = <$z>::column_result(value)?;
							<$nz>::new(i).ok_or(FromSqlError::OutOfRange(0))
						}
					}
				)
			);

			from_sql_integral!(i8);
			from_sql_integral!(i16);
			from_sql_integral!(i32);
			// from_sql_integral!(i64); // Not needed because the native type is i64.
			from_sql_integral!(isize);
			from_sql_integral!(u8);
			from_sql_integral!(u16);
			from_sql_integral!(u32);
			from_sql_integral!(u64);
			from_sql_integral!(usize);

			from_sql_integral!(non_zero std::num::NonZeroIsize, isize);
			from_sql_integral!(non_zero std::num::NonZeroI8, i8);
			from_sql_integral!(non_zero std::num::NonZeroI16, i16);
			from_sql_integral!(non_zero std::num::NonZeroI32, i32);
			from_sql_integral!(non_zero std::num::NonZeroI64, i64);
			#[cfg(feature = "i128_blob")]
			from_sql_integral!(non_zero std::num::NonZeroI128, i128);

			from_sql_integral!(non_zero std::num::NonZeroUsize, usize);
			from_sql_integral!(non_zero std::num::NonZeroU8, u8);
			from_sql_integral!(non_zero std::num::NonZeroU16, u16);
			from_sql_integral!(non_zero std::num::NonZeroU32, u32);
			from_sql_integral!(non_zero std::num::NonZeroU64, u64);
			// std::num::NonZeroU128 is not supported since u128 isn't either

			impl FromSql for i64 {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_i64()
				}
			}

			impl FromSql for f32 {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					match value {
						ValueRef::Integer(i) => Ok(i as Self),
						ValueRef::Real(f) => Ok(f as Self),
						_ => Err(FromSqlError::InvalidType),
					}
				}
			}

			impl FromSql for f64 {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					match value {
						ValueRef::Integer(i) => Ok(i as Self),
						ValueRef::Real(f) => Ok(f),
						_ => Err(FromSqlError::InvalidType),
					}
				}
			}

			impl FromSql for bool {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					i64::column_result(value).map(|i| i != 0)
				}
			}

			impl FromSql for String {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_str().map(ToString::to_string)
				}
			}

			impl FromSql for Box<str> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_str().map(Into::into)
				}
			}

			impl FromSql for std::rc::Rc<str> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_str().map(Into::into)
				}
			}

			impl FromSql for std::sync::Arc<str> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_str().map(Into::into)
				}
			}

			impl FromSql for Vec<u8> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_blob().map(<[u8]>::to_vec)
				}
			}

			impl FromSql for Box<[u8]> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_blob().map(Box::<[u8]>::from)
				}
			}

			impl FromSql for std::rc::Rc<[u8]> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_blob().map(std::rc::Rc::<[u8]>::from)
				}
			}

			impl FromSql for std::sync::Arc<[u8]> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					value.as_blob().map(std::sync::Arc::<[u8]>::from)
				}
			}

			impl<const N: usize> FromSql for [u8; N] {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					let slice = value.as_blob()?;
					slice.try_into().map_err(|_| FromSqlError::InvalidBlobSize {
						expected_size: N,
						blob_size: slice.len(),
					})
				}
			}

			#[cfg(feature = "i128_blob")]
			impl FromSql for i128 {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					let bytes = <[u8; 16]>::column_result(value)?;
					Ok(Self::from_be_bytes(bytes) ^ (1_i128 << 127))
				}
			}

			#[cfg(feature = "uuid")]
			impl FromSql for uuid::Uuid {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					let bytes = <[u8; 16]>::column_result(value)?;
					Ok(Self::from_u128(u128::from_be_bytes(bytes)))
				}
			}

			impl<T: FromSql> FromSql for Option<T> {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					match value {
						ValueRef::Null => Ok(None),
						_ => FromSql::column_result(value).map(Some),
					}
				}
			}

			impl<T: ?Sized> FromSql for Cow<'_, T>
			where
				T: ToOwned,
				T::Owned: FromSql,
			{
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					<T::Owned>::column_result(value).map(Cow::Owned)
				}
			}

			impl FromSql for Value {
				#[inline]
				fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
					Ok(value.into())
				}
			}

		} pub use self::from_sql::{ FromSql, FromSqlError, FromSqlResult };
		
		mod to_sql
		{
			/*!
			*/
			use ::
			{
				*,
			};
			/*
			use super::{Null, Value, ValueRef};
			#[cfg(feature = "array")]
			use crate::vtab::array::Array;
			use crate::{Error, Result};
			use std::borrow::Cow;
			*/
			/// `ToSqlOutput` represents the possible output types for implementers of the
			/// [`ToSql`] trait.
			#[derive(Clone, Debug, PartialEq)]
			#[non_exhaustive]
			pub enum ToSqlOutput<'a> {
				/// A borrowed SQLite-representable value.
				Borrowed(ValueRef<'a>),

				/// An owned SQLite-representable value.
				Owned(Value),

				/// A BLOB of the given length that is filled with
				/// zeroes.
				#[cfg(feature = "blob")]
				ZeroBlob(i32),

				/// n-th arg of an SQL scalar function
				#[cfg(feature = "functions")]
				Arg(usize),

				/// `feature = "array"`
				#[cfg(feature = "array")]
				Array(Array),
			}

			// Generically allow any type that can be converted into a ValueRef
			// to be converted into a ToSqlOutput as well.
			impl<'a, T: ?Sized> From<&'a T> for ToSqlOutput<'a>
			where
				&'a T: Into<ValueRef<'a>>,
			{
				#[inline]
				fn from(t: &'a T) -> Self {
					ToSqlOutput::Borrowed(t.into())
				}
			}

			// We cannot also generically allow any type that can be converted
			// into a Value to be converted into a ToSqlOutput because of
			// coherence rules (https://github.com/rust-lang/rust/pull/46192),
			// so we'll manually implement it for all the types we know can
			// be converted into Values.
			macro_rules! from_value(
				($t:ty) => (
					impl From<$t> for ToSqlOutput<'_> {
						#[inline]
						fn from(t: $t) -> Self { ToSqlOutput::Owned(t.into())}
					}
				);
				(non_zero $t:ty) => (
					impl From<$t> for ToSqlOutput<'_> {
						#[inline]
						fn from(t: $t) -> Self { ToSqlOutput::Owned(t.get().into())}
					}
				)
			);
			from_value!(String);
			from_value!(Null);
			from_value!(bool);
			from_value!(i8);
			from_value!(i16);
			from_value!(i32);
			from_value!(i64);
			from_value!(isize);
			from_value!(u8);
			from_value!(u16);
			from_value!(u32);
			from_value!(f32);
			from_value!(f64);
			from_value!(Vec<u8>);

			from_value!(non_zero std::num::NonZeroI8);
			from_value!(non_zero std::num::NonZeroI16);
			from_value!(non_zero std::num::NonZeroI32);
			from_value!(non_zero std::num::NonZeroI64);
			from_value!(non_zero std::num::NonZeroIsize);
			from_value!(non_zero std::num::NonZeroU8);
			from_value!(non_zero std::num::NonZeroU16);
			from_value!(non_zero std::num::NonZeroU32);

			// It would be nice if we could avoid the heap allocation (of the `Vec`) that
			// `i128` needs in `Into<Value>`, but it's probably fine for the moment, and not
			// worth adding another case to Value.
			#[cfg(feature = "i128_blob")]
			from_value!(i128);

			#[cfg(feature = "i128_blob")]
			from_value!(non_zero std::num::NonZeroI128);

			#[cfg(feature = "uuid")]
			from_value!(uuid::Uuid);

			impl ToSql for ToSqlOutput<'_> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(match *self {
						ToSqlOutput::Borrowed(v) => ToSqlOutput::Borrowed(v),
						ToSqlOutput::Owned(ref v) => ToSqlOutput::Borrowed(ValueRef::from(v)),

						#[cfg(feature = "blob")]
						ToSqlOutput::ZeroBlob(i) => ToSqlOutput::ZeroBlob(i),
						#[cfg(feature = "functions")]
						ToSqlOutput::Arg(i) => ToSqlOutput::Arg(i),
						#[cfg(feature = "array")]
						ToSqlOutput::Array(ref a) => ToSqlOutput::Array(a.clone()),
					})
				}
			}
			/// A trait for types that can be converted into SQLite values. Returns
			/// [`Error::ToSqlConversionFailure`] if the conversion fails.
			pub trait ToSql {
				/// Converts Rust value to SQLite value
				fn to_sql(&self) -> Result<ToSqlOutput<'_>>;
			}

			impl<T: ToSql + ToOwned + ?Sized> ToSql for Cow<'_, T> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					self.as_ref().to_sql()
				}
			}

			impl<T: ToSql + ?Sized> ToSql for Box<T> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					self.as_ref().to_sql()
				}
			}

			impl<T: ToSql + ?Sized> ToSql for std::rc::Rc<T> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					self.as_ref().to_sql()
				}
			}

			impl<T: ToSql + ?Sized> ToSql for std::sync::Arc<T> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					self.as_ref().to_sql()
				}
			}

			// We should be able to use a generic impl like this:
			//
			// impl<T: Copy> ToSql for T where T: Into<Value> {
			//     fn to_sql(&self) -> Result<ToSqlOutput> {
			//         Ok(ToSqlOutput::from((*self).into()))
			//     }
			// }
			//
			// instead of the following macro, but this runs afoul of
			// https://github.com/rust-lang/rust/issues/30191 and reports conflicting
			// implementations even when there aren't any.

			macro_rules! to_sql_self(
				($t:ty) => (
					impl ToSql for $t {
						#[inline]
						fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
							Ok(ToSqlOutput::from(*self))
						}
					}
				)
			);

			to_sql_self!(Null);
			to_sql_self!(bool);
			to_sql_self!(i8);
			to_sql_self!(i16);
			to_sql_self!(i32);
			to_sql_self!(i64);
			to_sql_self!(isize);
			to_sql_self!(u8);
			to_sql_self!(u16);
			to_sql_self!(u32);
			to_sql_self!(f32);
			to_sql_self!(f64);

			to_sql_self!(std::num::NonZeroI8);
			to_sql_self!(std::num::NonZeroI16);
			to_sql_self!(std::num::NonZeroI32);
			to_sql_self!(std::num::NonZeroI64);
			to_sql_self!(std::num::NonZeroIsize);
			to_sql_self!(std::num::NonZeroU8);
			to_sql_self!(std::num::NonZeroU16);
			to_sql_self!(std::num::NonZeroU32);

			#[cfg(feature = "i128_blob")]
			to_sql_self!(i128);

			#[cfg(feature = "i128_blob")]
			to_sql_self!(std::num::NonZeroI128);

			#[cfg(feature = "uuid")]
			to_sql_self!(uuid::Uuid);

			macro_rules! to_sql_self_fallible(
				($t:ty) => (
					impl ToSql for $t {
						#[inline]
						fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
							Ok(ToSqlOutput::Owned(Value::Integer(
								i64::try_from(*self).map_err(
									// TODO: Include the values in the error message.
									|err| Error::ToSqlConversionFailure(err.into())
								)?
							)))
						}
					}
				);
				(non_zero $t:ty) => (
					impl ToSql for $t {
						#[inline]
						fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
							Ok(ToSqlOutput::Owned(Value::Integer(
								i64::try_from(self.get()).map_err(
									// TODO: Include the values in the error message.
									|err| Error::ToSqlConversionFailure(err.into())
								)?
							)))
						}
					}
				)
			);

			// Special implementations for usize and u64 because these conversions can fail.
			to_sql_self_fallible!(u64);
			to_sql_self_fallible!(usize);
			to_sql_self_fallible!(non_zero std::num::NonZeroU64);
			to_sql_self_fallible!(non_zero std::num::NonZeroUsize);

			impl<T: ?Sized> ToSql for &'_ T
			where
				T: ToSql,
			{
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					(*self).to_sql()
				}
			}

			impl ToSql for String {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(ToSqlOutput::from(self.as_str()))
				}
			}

			impl ToSql for str {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(ToSqlOutput::from(self))
				}
			}

			impl ToSql for Vec<u8> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(ToSqlOutput::from(self.as_slice()))
				}
			}

			impl<const N: usize> ToSql for [u8; N] {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(ToSqlOutput::from(&self[..]))
				}
			}

			impl ToSql for [u8] {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(ToSqlOutput::from(self))
				}
			}

			impl ToSql for Value {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					Ok(ToSqlOutput::from(self))
				}
			}

			impl<T: ToSql> ToSql for Option<T> {
				#[inline]
				fn to_sql(&self) -> Result<ToSqlOutput<'_>> {
					match *self {
						None => Ok(ToSqlOutput::from(Null)),
						Some(ref t) => t.to_sql(),
					}
				}
			}

		}
		
		mod value
		{
			/*!
			*/
			use ::
			{
				*,
			};
			/*
			use super::{Null, Type};
			*/
			/// Owning [dynamic type value](http://sqlite.org/datatype3.html). Value's type is typically
			/// dictated by SQLite (not by the caller).
			///
			/// See [`ValueRef`](crate::types::ValueRef) for a non-owning dynamic type
			/// value.
			#[derive(Clone, Debug, PartialEq)]
			pub enum Value {
				/// The value is a `NULL` value.
				Null,
				/// The value is a signed integer.
				Integer(i64),
				/// The value is a floating point number.
				Real(f64),
				/// The value is a text string.
				Text(String),
				/// The value is a blob of data
				Blob(Vec<u8>),
			}

			impl From<Null> for Value {
				#[inline]
				fn from(_: Null) -> Self {
					Self::Null
				}
			}

			impl From<bool> for Value {
				#[inline]
				fn from(i: bool) -> Self {
					Self::Integer(i as i64)
				}
			}

			impl From<isize> for Value {
				#[inline]
				fn from(i: isize) -> Self {
					Self::Integer(i as i64)
				}
			}

			#[cfg(feature = "i128_blob")]
			impl From<i128> for Value {
				#[inline]
				fn from(i: i128) -> Self {
					// We store these biased (e.g. with the most significant bit flipped)
					// so that comparisons with negative numbers work properly.
					Self::Blob(i128::to_be_bytes(i ^ (1_i128 << 127)).to_vec())
				}
			}

			#[cfg(feature = "uuid")]
			impl From<uuid::Uuid> for Value {
				#[inline]
				fn from(id: uuid::Uuid) -> Self {
					Self::Blob(id.as_bytes().to_vec())
				}
			}

			macro_rules! from_i64(
				($t:ty) => (
					impl From<$t> for Value {
						#[inline]
						fn from(i: $t) -> Value {
							Value::Integer(i64::from(i))
						}
					}
				)
			);

			from_i64!(i8);
			from_i64!(i16);
			from_i64!(i32);
			from_i64!(u8);
			from_i64!(u16);
			from_i64!(u32);

			impl From<i64> for Value {
				#[inline]
				fn from(i: i64) -> Self {
					Self::Integer(i)
				}
			}

			impl From<f32> for Value {
				#[inline]
				fn from(f: f32) -> Self {
					Self::Real(f.into())
				}
			}

			impl From<f64> for Value {
				#[inline]
				fn from(f: f64) -> Self {
					Self::Real(f)
				}
			}

			impl From<String> for Value {
				#[inline]
				fn from(s: String) -> Self {
					Self::Text(s)
				}
			}

			impl From<Vec<u8>> for Value {
				#[inline]
				fn from(v: Vec<u8>) -> Self {
					Self::Blob(v)
				}
			}

			impl<T> From<Option<T>> for Value
			where
				T: Into<Self>,
			{
				#[inline]
				fn from(v: Option<T>) -> Self {
					match v {
						Some(x) => x.into(),
						None => Self::Null,
					}
				}
			}

			impl Value {
				/// Returns SQLite fundamental datatype.
				#[inline]
				#[must_use]
				pub fn data_type(&self) -> Type {
					match *self {
						Self::Null => Type::Null,
						Self::Integer(_) => Type::Integer,
						Self::Real(_) => Type::Real,
						Self::Text(_) => Type::Text,
						Self::Blob(_) => Type::Blob,
					}
				}
			}

		}
		
		mod value_ref
		{
			/*!
			*/
			use ::
			{
				*,
			};
			/*
			use super::{Type, Value};
			use crate::types::{FromSqlError, FromSqlResult};
			*/
			/// A non-owning [dynamic type value](http://sqlite.org/datatype3.html). Typically, the
			/// memory backing this value is owned by SQLite.
			///
			/// See [`Value`](Value) for an owning dynamic type value.
			#[derive(Copy, Clone, Debug, PartialEq)]
			pub enum ValueRef<'a> {
				/// The value is a `NULL` value.
				Null,
				/// The value is a signed integer.
				Integer(i64),
				/// The value is a floating point number.
				Real(f64),
				/// The value is a text string.
				Text(&'a [u8]),
				/// The value is a blob of data
				Blob(&'a [u8]),
			}

			impl ValueRef<'_> {
				/// Returns SQLite fundamental datatype.
				#[inline]
				#[must_use]
				pub fn data_type(&self) -> Type {
					match *self {
						ValueRef::Null => Type::Null,
						ValueRef::Integer(_) => Type::Integer,
						ValueRef::Real(_) => Type::Real,
						ValueRef::Text(_) => Type::Text,
						ValueRef::Blob(_) => Type::Blob,
					}
				}
			}

			impl<'a> ValueRef<'a> {
				/// If `self` is case `Integer`, returns the integral value. Otherwise,
				/// returns [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_i64(&self) -> FromSqlResult<i64> {
					match *self {
						ValueRef::Integer(i) => Ok(i),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Null` returns None.
				/// If `self` is case `Integer`, returns the integral value.
				/// Otherwise, returns [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_i64_or_null(&self) -> FromSqlResult<Option<i64>> {
					match *self {
						ValueRef::Null => Ok(None),
						ValueRef::Integer(i) => Ok(Some(i)),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Real`, returns the floating point value. Otherwise,
				/// returns [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_f64(&self) -> FromSqlResult<f64> {
					match *self {
						ValueRef::Real(f) => Ok(f),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Null` returns None.
				/// If `self` is case `Real`, returns the floating point value.
				/// Otherwise, returns [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_f64_or_null(&self) -> FromSqlResult<Option<f64>> {
					match *self {
						ValueRef::Null => Ok(None),
						ValueRef::Real(f) => Ok(Some(f)),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Text`, returns the string value. Otherwise, returns
				/// [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_str(&self) -> FromSqlResult<&'a str> {
					match *self {
						ValueRef::Text(t) => std::str::from_utf8(t).map_err(FromSqlError::other),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Null` returns None.
				/// If `self` is case `Text`, returns the string value.
				/// Otherwise, returns [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_str_or_null(&self) -> FromSqlResult<Option<&'a str>> {
					match *self {
						ValueRef::Null => Ok(None),
						ValueRef::Text(t) => std::str::from_utf8(t)
							.map_err(FromSqlError::other)
							.map(Some),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Blob`, returns the byte slice. Otherwise, returns
				/// [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_blob(&self) -> FromSqlResult<&'a [u8]> {
					match *self {
						ValueRef::Blob(b) => Ok(b),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Null` returns None.
				/// If `self` is case `Blob`, returns the byte slice.
				/// Otherwise, returns [`Err(FromSqlError::InvalidType)`](crate::types::from_sql::FromSqlError::InvalidType).
				#[inline]
				pub fn as_blob_or_null(&self) -> FromSqlResult<Option<&'a [u8]>> {
					match *self {
						ValueRef::Null => Ok(None),
						ValueRef::Blob(b) => Ok(Some(b)),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// Returns the byte slice that makes up this `ValueRef` if it's either
				/// [`ValueRef::Blob`] or [`ValueRef::Text`].
				#[inline]
				pub fn as_bytes(&self) -> FromSqlResult<&'a [u8]> {
					match self {
						ValueRef::Text(s) | ValueRef::Blob(s) => Ok(s),
						_ => Err(FromSqlError::InvalidType),
					}
				}

				/// If `self` is case `Null` returns None.
				/// If `self` is [`ValueRef::Blob`] or [`ValueRef::Text`] returns the byte
				/// slice that makes up this value
				#[inline]
				pub fn as_bytes_or_null(&self) -> FromSqlResult<Option<&'a [u8]>> {
					match *self {
						ValueRef::Null => Ok(None),
						ValueRef::Text(s) | ValueRef::Blob(s) => Ok(Some(s)),
						_ => Err(FromSqlError::InvalidType),
					}
				}
			}

			impl From<ValueRef<'_>> for Value {
				#[inline]
				#[track_caller]
				fn from(borrowed: ValueRef<'_>) -> Self {
					match borrowed {
						ValueRef::Null => Self::Null,
						ValueRef::Integer(i) => Self::Integer(i),
						ValueRef::Real(r) => Self::Real(r),
						ValueRef::Text(s) => {
							let s = std::str::from_utf8(s).expect("invalid UTF-8");
							Self::Text(s.to_string())
						}
						ValueRef::Blob(b) => Self::Blob(b.to_vec()),
					}
				}
			}

			impl<'a> From<&'a str> for ValueRef<'a> {
				#[inline]
				fn from(s: &str) -> ValueRef<'_> {
					ValueRef::Text(s.as_bytes())
				}
			}

			impl<'a> From<&'a [u8]> for ValueRef<'a> {
				#[inline]
				fn from(s: &[u8]) -> ValueRef<'_> {
					ValueRef::Blob(s)
				}
			}

			impl<'a> From<&'a Value> for ValueRef<'a> {
				#[inline]
				fn from(value: &'a Value) -> Self {
					match *value {
						Value::Null => ValueRef::Null,
						Value::Integer(i) => ValueRef::Integer(i),
						Value::Real(r) => ValueRef::Real(r),
						Value::Text(ref s) => ValueRef::Text(s.as_bytes()),
						Value::Blob(ref b) => ValueRef::Blob(b),
					}
				}
			}

			impl<T> From<Option<T>> for ValueRef<'_>
			where
				T: Into<Self>,
			{
				#[inline]
				fn from(s: Option<T>) -> Self {
					match s {
						Some(x) => x.into(),
						None => ValueRef::Null,
					}
				}
			}

			#[cfg(any(
				feature = "functions",
				feature = "session",
				feature = "vtab",
				feature = "preupdate_hook"
			))]
			impl ValueRef<'_> {
				pub(crate) unsafe fn from_value(value: *mut crate::ffi::sqlite3_value) -> Self {
					use crate::ffi;
					use std::slice::from_raw_parts;

					match ffi::sqlite3_value_type(value) {
						ffi::SQLITE_NULL => ValueRef::Null,
						ffi::SQLITE_INTEGER => ValueRef::Integer(ffi::sqlite3_value_int64(value)),
						ffi::SQLITE_FLOAT => ValueRef::Real(ffi::sqlite3_value_double(value)),
						ffi::SQLITE_TEXT => {
							let text = ffi::sqlite3_value_text(value);
							let len = ffi::sqlite3_value_bytes(value);
							assert!(
								!text.is_null(),
								"unexpected SQLITE_TEXT value type with NULL data"
							);
							let s = from_raw_parts(text.cast::<u8>(), len as usize);
							ValueRef::Text(s)
						}
						ffi::SQLITE_BLOB => {
							let (blob, len) = (
								ffi::sqlite3_value_blob(value),
								ffi::sqlite3_value_bytes(value),
							);

							assert!(
								len >= 0,
								"unexpected negative return from sqlite3_value_bytes"
							);
							if len > 0 {
								assert!(
									!blob.is_null(),
									"unexpected SQLITE_BLOB value type with NULL data"
								);
								ValueRef::Blob(from_raw_parts(blob.cast::<u8>(), len as usize))
							} else {
								// The return value from sqlite3_value_blob() for a zero-length BLOB
								// is a NULL pointer.
								ValueRef::Blob(&[])
							}
						}
						_ => unreachable!("sqlite3_value_type returned invalid value"),
					}
				}

				// TODO sqlite3_value_nochange // 3.22.0 & VTab xUpdate
				// TODO sqlite3_value_frombind // 3.28.0
			}

		}
		/// Empty struct that can be used to fill in a query parameter as `NULL`.
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// # use rusqlite::types::{Null};
		///
		/// fn insert_null(conn: &Connection) -> Result<usize> {
		///     conn.execute("INSERT INTO people (name) VALUES (?1)", [Null])
		/// }
		/// ```
		#[derive(Copy, Clone)]
		pub struct Null;

		/// SQLite data types.
		/// See [Fundamental Datatypes](https://sqlite.org/c3ref/c_blob.html).
		#[derive(Copy, Clone, Debug, PartialEq, Eq)]
		pub enum Type {
			/// NULL
			Null,
			/// 64-bit signed integer
			Integer,
			/// 64-bit IEEE floating point number
			Real,
			/// String
			Text,
			/// BLOB
			Blob,
		}

		impl fmt::Display for Type {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				match *self {
					Self::Null => f.pad("Null"),
					Self::Integer => f.pad("Integer"),
					Self::Real => f.pad("Real"),
					Self::Text => f.pad("Text"),
					Self::Blob => f.pad("Blob"),
				}
			}
		}

    } use self::types::{ ToSql, ValueRef };

	pub mod version
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
		use crate::ffi;
		use std::ffi::CStr;
        */
        /// Returns the SQLite version as an integer; e.g., `3016002` for version
		/// 3.16.2.
		///
		/// See [`sqlite3_libversion_number()`](https://www.sqlite.org/c3ref/libversion.html).
		#[inline]
		#[must_use]
		pub fn version_number() -> i32 {
			unsafe { ffi::sqlite3_libversion_number() }
		}

		/// Returns the SQLite version as a string; e.g., `"3.16.2"` for version 3.16.2.
		///
		/// See [`sqlite3_libversion()`](https://www.sqlite.org/c3ref/libversion.html).
		///
		/// # Panics
		///
		/// Panics when version is not valid UTF-8.
		#[inline]
		#[must_use]
		pub fn version() -> &'static str {
			let cstr = unsafe { CStr::from_ptr(ffi::sqlite3_libversion()) };
			cstr.to_str()
				.expect("SQLite version string is not valid UTF8 ?!")
		}
    } pub use self::version::*;

	pub mod util
    {
        /*!
        Internal utilities */
        use ::
        {
            *,
        };
        /*
		pub(crate) use param_cache::ParamIndexCache;
		pub(crate) use small_cstr::SmallCString;
		pub(crate) use sqlite_string::{alloc, SqliteMallocString};
		use crate::Result;
		use std::ffi::CStr;
        */
        pub mod param_cache
		{
			/*!
			*/
			use ::
			{
				*,
			};
			/*
			use super::SmallCString;
			use std::cell::RefCell;
			use std::collections::BTreeMap;
			*/
			/// Maps parameter names to parameter indices.
			#[derive(Default, Clone, Debug)]
			// BTreeMap seems to do better here unless we want to pull in a custom hash
			// function.
			pub(crate) struct ParamIndexCache(RefCell<BTreeMap<SmallCString, usize>>);

			impl ParamIndexCache {
				pub fn get_or_insert_with<F>(&self, s: &str, func: F) -> Option<usize>
				where
					F: FnOnce(&std::ffi::CStr) -> Option<usize>,
				{
					let mut cache = self.0.borrow_mut();
					// Avoid entry API, needs allocation to test membership.
					if let Some(v) = cache.get(s) {
						return Some(*v);
					}
					// If there's an internal nul in the name it couldn't have been a
					// parameter, so early return here is ok.
					let name = SmallCString::new(s).ok()?;
					let val = func(&name)?;
					cache.insert(name, val);
					Some(val)
				}
			}

		}
		
		pub mod small_cstr
		{
			/*!
			*/
			use ::
			{
				*,
			};
			/*
			use smallvec::{smallvec, SmallVec};
			use std::ffi::{CStr, CString, NulError};
			*/
			/// Similar to `std::ffi::CString`, but avoids heap allocating if the string is
			/// small enough. Also guarantees it's input is UTF-8 -- used for cases where we
			/// need to pass a NUL-terminated string to SQLite, and we have a `&str`.
			#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
			pub struct SmallCString(SmallVec<[u8; 16]>);

			impl SmallCString {
				#[inline]
				pub fn new(s: &str) -> Result<Self, NulError> {
					if s.as_bytes().contains(&0_u8) {
						return Err(Self::fabricate_nul_error(s));
					}
					let mut buf = SmallVec::with_capacity(s.len() + 1);
					buf.extend_from_slice(s.as_bytes());
					buf.push(0);
					let res = Self(buf);
					res.debug_checks();
					Ok(res)
				}

				#[inline]
				pub fn as_str(&self) -> &str {
					self.debug_checks();
					// Constructor takes a &str so this is safe.
					unsafe { std::str::from_utf8_unchecked(self.as_bytes_without_nul()) }
				}

				/// Get the bytes not including the NUL terminator. E.g. the bytes which
				/// make up our `str`:
				/// - `SmallCString::new("foo").as_bytes_without_nul() == b"foo"`
				/// - `SmallCString::new("foo").as_bytes_with_nul() == b"foo\0"`
				#[inline]
				pub fn as_bytes_without_nul(&self) -> &[u8] {
					self.debug_checks();
					&self.0[..self.len()]
				}

				/// Get the bytes behind this str *including* the NUL terminator. This
				/// should never return an empty slice.
				#[inline]
				pub fn as_bytes_with_nul(&self) -> &[u8] {
					self.debug_checks();
					&self.0
				}

				#[inline]
				#[cfg(debug_assertions)]
				fn debug_checks(&self) {
					debug_assert_ne!(self.0.len(), 0);
					debug_assert_eq!(self.0[self.0.len() - 1], 0);
					let strbytes = &self.0[..(self.0.len() - 1)];
					debug_assert!(!strbytes.contains(&0));
					debug_assert!(std::str::from_utf8(strbytes).is_ok());
				}

				#[inline]
				#[cfg(not(debug_assertions))]
				fn debug_checks(&self) {}

				#[inline]
				pub fn len(&self) -> usize {
					debug_assert_ne!(self.0.len(), 0);
					self.0.len() - 1
				}

				#[inline]
				#[allow(unused)] // clippy wants this function.
				pub fn is_empty(&self) -> bool {
					self.len() == 0
				}

				#[inline]
				pub fn as_cstr(&self) -> &CStr {
					let bytes = self.as_bytes_with_nul();
					debug_assert!(CStr::from_bytes_with_nul(bytes).is_ok());
					unsafe { CStr::from_bytes_with_nul_unchecked(bytes) }
				}

				#[cold]
				fn fabricate_nul_error(b: &str) -> NulError {
					CString::new(b).unwrap_err()
				}
			}

			impl Default for SmallCString {
				#[inline]
				fn default() -> Self {
					Self(smallvec![0])
				}
			}

			impl std::fmt::Debug for SmallCString {
				fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					f.debug_tuple("SmallCString").field(&self.as_str()).finish()
				}
			}

			impl std::ops::Deref for SmallCString {
				type Target = CStr;

				#[inline]
				fn deref(&self) -> &CStr {
					self.as_cstr()
				}
			}

			impl std::borrow::Borrow<str> for SmallCString {
				#[inline]
				fn borrow(&self) -> &str {
					self.as_str()
				}
			}

		}
		
		pub mod sqlite_string
		{
			/*!
			Used when either vtab or modern-sqlite is on. */
			use ::
			{
				*,
			};
			/*
			// This is used when either vtab or modern-sqlite is on. Different methods are
			// used in each feature. Avoid having to track this for each function. We will
			// still warn for anything that's not used by either, though.
			#![cfg_attr(not(feature = "vtab"), allow(dead_code))]
			use crate::ffi;
			use std::ffi::{c_char, c_int, CStr};
			use std::marker::PhantomData;
			use std::ptr::NonNull;
			*/
			// Space to hold this string must be obtained
			// from an SQLite memory allocation function
			pub(crate) fn alloc(s: &str) -> *mut c_char {
				SqliteMallocString::from_str(s).into_raw()
			}
			/// A string we own that's allocated on the SQLite heap. Automatically calls
			/// `sqlite3_free` when dropped, unless `into_raw` (or `into_inner`) is called
			/// on it. If constructed from a rust string, `sqlite3_malloc` is used.
			///
			/// It has identical representation to a nonnull `*mut c_char`, so you can use
			/// it transparently as one. It's nonnull, so Option<SqliteMallocString> can be
			/// used for nullable ones (it's still just one pointer).
			///
			/// Most strings shouldn't use this! Only places where the string needs to be
			/// freed with `sqlite3_free`. This includes `sqlite3_extended_sql` results,
			/// some error message pointers... Note that misuse is extremely dangerous!
			///
			/// Note that this is *not* a lossless interface. Incoming strings with internal
			/// NULs are modified, and outgoing strings which are non-UTF8 are modified.
			/// This seems unavoidable -- it tries very hard to not panic.
			#[repr(transparent)]
			pub(crate) struct SqliteMallocString {
				ptr: NonNull<c_char>,
				_boo: PhantomData<Box<[c_char]>>,
			}
			// This is owned data for a primitive type, and thus it's safe to implement
			// these. That said, nothing needs them, and they make things easier to misuse.

			// unsafe impl Send for SqliteMallocString {}
			// unsafe impl Sync for SqliteMallocString {}

			impl SqliteMallocString {
				/// SAFETY: Caller must be certain that `m` a nul-terminated c string
				/// allocated by `sqlite3_malloc`, and that SQLite expects us to free it!
				#[inline]
				pub(crate) unsafe fn from_raw_nonnull(ptr: NonNull<c_char>) -> Self {
					Self {
						ptr,
						_boo: PhantomData,
					}
				}

				/// SAFETY: Caller must be certain that `m` a nul-terminated c string
				/// allocated by `sqlite3_malloc`, and that SQLite expects us to free it!
				#[inline]
				pub(crate) unsafe fn from_raw(ptr: *mut c_char) -> Option<Self> {
					NonNull::new(ptr).map(|p| Self::from_raw_nonnull(p))
				}

				/// Get the pointer behind `self`. After this is called, we no longer manage
				/// it.
				#[inline]
				pub(crate) fn into_inner(self) -> NonNull<c_char> {
					let p = self.ptr;
					std::mem::forget(self);
					p
				}

				/// Get the pointer behind `self`. After this is called, we no longer manage
				/// it.
				#[inline]
				pub(crate) fn into_raw(self) -> *mut c_char {
					self.into_inner().as_ptr()
				}

				/// Borrow the pointer behind `self`. We still manage it when this function
				/// returns. If you want to relinquish ownership, use `into_raw`.
				#[inline]
				pub(crate) fn as_ptr(&self) -> *const c_char {
					self.ptr.as_ptr()
				}

				#[inline]
				pub(crate) fn as_cstr(&self) -> &CStr {
					unsafe { CStr::from_ptr(self.as_ptr()) }
				}

				#[inline]
				pub(crate) fn to_string_lossy(&self) -> std::borrow::Cow<'_, str> {
					self.as_cstr().to_string_lossy()
				}

				/// Convert `s` into a SQLite string.
				///
				/// This should almost never be done except for cases like error messages or
				/// other strings that SQLite frees.
				///
				/// If `s` contains internal NULs, we'll replace them with
				/// `NUL_REPLACE_CHAR`.
				///
				/// Except for `debug_assert`s which may trigger during testing, this
				/// function never panics. If we hit integer overflow or the allocation
				/// fails, we call `handle_alloc_error` which aborts the program after
				/// calling a global hook.
				///
				/// This means it's safe to use in extern "C" functions even outside
				/// `catch_unwind`.
				pub(crate) fn from_str(s: &str) -> Self {
					let s = if s.as_bytes().contains(&0) {
						std::borrow::Cow::Owned(make_nonnull(s))
					} else {
						std::borrow::Cow::Borrowed(s)
					};
					debug_assert!(!s.as_bytes().contains(&0));
					let bytes: &[u8] = s.as_ref().as_bytes();
					let src_ptr: *const c_char = bytes.as_ptr().cast();
					let src_len = bytes.len();
					let maybe_len_plus_1 = s.len().checked_add(1).and_then(|v| c_int::try_from(v).ok());
					unsafe {
						let res_ptr = maybe_len_plus_1
							.and_then(|len_to_alloc| {
								// `>` because we added 1.
								debug_assert!(len_to_alloc > 0);
								debug_assert_eq!((len_to_alloc - 1) as usize, src_len);
								NonNull::new(ffi::sqlite3_malloc(len_to_alloc).cast::<c_char>())
							})
							.unwrap_or_else(|| {
								use std::alloc::{handle_alloc_error, Layout};
								// Report via handle_alloc_error so that it can be handled with any
								// other allocation errors and properly diagnosed.
								//
								// This is safe:
								// - `align` is never 0
								// - `align` is always a power of 2.
								// - `size` needs no realignment because it's guaranteed to be aligned
								//   (everything is aligned to 1)
								// - `size` is also never zero, although this function doesn't actually require
								//   it now.
								let len = s.len().saturating_add(1).min(isize::MAX as usize);
								let layout = Layout::from_size_align_unchecked(len, 1);
								// Note: This call does not return.
								handle_alloc_error(layout);
							});
						let buf: *mut c_char = res_ptr.as_ptr().cast::<c_char>();
						src_ptr.copy_to_nonoverlapping(buf, src_len);
						buf.add(src_len).write(0);
						debug_assert_eq!(std::ffi::CStr::from_ptr(res_ptr.as_ptr()).to_bytes(), bytes);
						Self::from_raw_nonnull(res_ptr)
					}
				}
			}

			const NUL_REPLACE: &str = "␀";

			#[cold]
			fn make_nonnull(v: &str) -> String {
				v.replace('\0', NUL_REPLACE)
			}

			impl Drop for SqliteMallocString {
				#[inline]
				fn drop(&mut self) {
					unsafe { ffi::sqlite3_free(self.ptr.as_ptr().cast()) };
				}
			}

		}
		
		pub enum Named<'a> {
			Small(SmallCString),
			C(&'a CStr),
		}
		
		impl std::ops::Deref for Named<'_> {
			type Target = CStr;
			#[inline] fn deref(&self) -> &CStr {
				match self {
					Named::Small(s) => s.as_cstr(),
					Named::C(s) => s,
				}
			}
		}

		/// Database, table, column, collation, function, module, vfs name
		pub trait Name: std::fmt::Debug {
			/// As C string
			fn as_cstr(&self) -> Result<Named<'_>>;
		}
		impl Name for &str {
			fn as_cstr(&self) -> Result<Named<'_>>
            {
				let ss = SmallCString::new(self)?;
				Ok(Named::Small(ss))
			}
		}
		impl Name for &CStr {
			#[inline] fn as_cstr(&self) -> Result<Named<'_>> {
				Ok(Named::C(self))
			}
		}

    } pub use self::util::Name;
	/// Number of cached prepared statements we'll hold on to.
	const STATEMENT_CACHE_DEFAULT_CAPACITY: usize = 16;	
	/// A typedef of the result returned by many methods.
	pub type Result<T, E = Error> = result::Result<T, E>;
	/// See the [method documentation](#tymethod.optional).
	pub trait OptionalExtension<T>
    {
		/// Converts a `Result<T>` into a `Result<Option<T>>`.
		fn optional(self) -> Result<Option<T>>;
	}

	impl<T> OptionalExtension<T> for Result<T>
    {
		fn optional(self) -> Result<Option<T>>
        {
			match self
            {
				Ok(value) => Ok(Some(value)),
				Err(Error::QueryReturnedNoRows) => Ok(None),
				Err(e) => Err(e),
			}
		}
	}

	unsafe fn errmsg_to_string(errmsg: *const c_char) -> String
    { CStr::from_ptr(errmsg).to_string_lossy().into_owned() } 
	/// Returns `Ok((string ptr, len as c_int, SQLITE_STATIC | SQLITE_TRANSIENT))` normally.
	fn str_for_sqlite(s: &[u8]) -> Result<(*const c_char, c_int, ffi::sqlite3_destructor_type)>
    {
		let len = len_as_c_int(s.len())?;
		let (ptr, dtor_info) = if len != 0 {
			(s.as_ptr().cast::<c_char>(), ffi::SQLITE_TRANSIENT())
		} else {
			("".as_ptr().cast::<c_char>(), ffi::SQLITE_STATIC())
		};
		Ok((ptr, len, dtor_info))
	}
    
	fn len_as_c_int(len: usize) -> Result<c_int>
    {
		if len >= (c_int::MAX as usize) {
			Err(err!(ffi::SQLITE_TOOBIG))
		} else {
			Ok(len as c_int)
		}
	}

	#[cfg(unix)] fn path_to_cstring(p: &Path) -> Result<CString>
    {
		use ::os::unix::ffi::OsStrExt;
		Ok(CString::new(p.as_os_str().as_bytes())?)
	}

	#[cfg(not(unix))] fn path_to_cstring(p: &Path) -> Result<CString>
    {
		let s = p.to_str().ok_or_else(|| Error::InvalidPath(p.to_owned()))?;
		Ok(CString::new(s)?)
	}
	/// Shorthand for `Main` database.
	//pub const MAIN_DB: &CStr = c"main";
	/// Shorthand for `Temp` database.
	//pub const TEMP_DB: &CStr = c"temp";
	/// A connection to a SQLite database.
	pub struct Connection
    {
		db: RefCell<InnerConnection>,
		cache: StatementCache,
		transaction_behavior: TransactionBehavior,
	}

	unsafe impl Send for Connection {}

	impl Drop for Connection
    {
		#[inline] fn drop(&mut self) { self.flush_prepared_statement_cache(); }
	}

	impl Connection
    {
		/// Open a new connection to a SQLite database.
		#[inline] pub fn open<P: AsRef<Path>>(path: P) -> Result<Self>
        {
			let flags = OpenFlags::default();
			Self::open_with_flags(path, flags)
		}
		/// Open a new connection to an in-memory SQLite database.
		#[inline] pub fn open_in_memory() -> Result<Self>
        {
			let flags = OpenFlags::default();
			Self::open_in_memory_with_flags(flags)
		}
		/// Open a new connection to a SQLite database.
		#[inline] pub fn open_with_flags<P: AsRef<Path>>(path: P, flags: OpenFlags) -> Result<Self>
        {
			let c_path = path_to_cstring(path.as_ref())?;
			InnerConnection::open_with_flags(&c_path, flags, None).map(|db| Self
            {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}
		/// Open a new connection to a SQLite database using the specific flags and vfs name.
		#[inline] pub fn open_with_flags_and_vfs<P: AsRef<Path>, V: Name>
        ( 
            path:P,
            flags:OpenFlags,
            vfs:V
        ) -> Result<Self>
        {
			let c_path = path_to_cstring(path.as_ref())?;
			let c_vfs = vfs.as_cstr()?;
			InnerConnection::open_with_flags(&c_path, flags, Some(&c_vfs)).map(|db| Self
            {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}
		/// Open a new connection to an in-memory SQLite database.
		#[inline] pub fn open_in_memory_with_flags(flags: OpenFlags) -> Result<Self>
        { Self::open_with_flags(":memory:", flags) }
		/// Open a new connection to an in-memory SQLite database using the specific flags and vfs name.
		#[inline] pub fn open_in_memory_with_flags_and_vfs<V: Name>(flags: OpenFlags, vfs: V) -> Result<Self>
        { Self::open_with_flags_and_vfs(":memory:", flags, vfs) }
		/// Convenience method to run multiple SQL statements (that cannot take any parameters).
		pub fn execute_batch(&self, sql: &str) -> Result<()>
        {
			let mut sql = sql;
			while !sql.is_empty()
            {
				let (stmt, tail) = self
					.db
					.borrow_mut()
					.prepare(self, sql, PrepFlags::default())?;
				if !stmt.stmt.is_null() && stmt.step()? {
					if false {
						return Err(Error::ExecuteReturnedResults);
					}
				}
				if tail == 0 || tail >= sql.len() {
					break;
				}
				sql = &sql[tail..];
			}
			Ok(())
		}
		/// Convenience method to prepare and execute a single SQL statement.
		#[inline] pub fn execute<P: Params>(&self, sql: &str, params: P) -> Result<usize>
        { self.prepare(sql).and_then(|mut stmt| stmt.execute(params)) }
		/// Returns the path to the database file, if one exists and is known.
		#[inline] pub fn path(&self) -> Option<&str>
        {
			unsafe { crate::inner_connection::db_filename(::marker::PhantomData, self.handle(), MAIN_DB) }
		}
		/// Attempts to free as much heap memory as possible from the database connection.
		#[inline] pub fn release_memory(&self) -> Result<()> { self.db.borrow_mut().release_memory() }
		/// Get the SQLite rowid of the most recent successful INSERT.
		#[inline] pub fn last_insert_rowid(&self) -> i64 { self.db.borrow_mut().last_insert_rowid() }
		/// Convenience method to execute a query that is expected to return a single row.
		#[inline] pub fn query_row<T, P, F>(&self, sql: &str, params: P, f: F) -> Result<T> where
        P: Params,
        F: FnOnce(&Row<'_>) -> Result<T>,
		{
			let mut stmt = self.prepare(sql)?;
			stmt.query_row(params, f)
		}
		/// Convenience method to execute a query that is expected to return exactly one row.
		pub fn query_one<T, P, F>(&self, sql: &str, params: P, f: F) -> Result<T> where
        P: Params,
        F: FnOnce(&Row<'_>) -> Result<T>,
		{
			let mut stmt = self.prepare(sql)?;
			stmt.query_one(params, f)
		}
		/// Convenience method to execute a query that is expected to return a single row,
        /// and execute a mapping via `f` on that returned row with the possibility of failure.
		#[inline] pub fn query_row_and_then<T, E, P, F>(&self, sql: &str, params: P, f: F) -> Result<T, E> where
        P: Params,
        F: FnOnce(&Row<'_>) -> Result<T, E>,
        E: From<Error>
		{
			let mut stmt = self.prepare(sql)?;
			let mut rows = stmt.query(params)?;
			rows.get_expected_row().map_err(E::from).and_then(f)
		}
		/// Prepare a SQL statement for execution.
		#[inline] pub fn prepare(&self, sql: &str) -> Result<Statement<'_>>
        {
			self.prepare_with_flags(sql, PrepFlags::default())
		}
		/// Prepare a SQL statement for execution.
		#[inline] pub fn prepare_with_flags(&self, sql: &str, flags: PrepFlags) -> Result<Statement<'_>>
        {
			let (stmt, tail) = self.db.borrow_mut().prepare(self, sql, flags)?;
			if tail != 0 && !self.prepare(&sql[tail..])?.stmt.is_null() {
				Err(Error::MultipleStatement)
			} else {
				Ok(stmt)
			}
		}
		/// Close the SQLite connection.
		#[inline] pub fn close(self) -> Result<(), (Self, Error)>
        {
			self.flush_prepared_statement_cache();
			let r = self.db.borrow_mut().close();
			r.map_err(move |err| (self, err))
		}
		/// Get access to the underlying SQLite database connection handle.
		#[inline] pub unsafe fn handle(&self) -> *mut ffi::sqlite3
        {
			self.db.borrow().db()
		}
		/// Create a `Connection` from a raw handle.
		#[inline] pub unsafe fn from_handle(db: *mut ffi::sqlite3) -> Result<Self>
        {
			let db = InnerConnection::new(db, false);
			Ok(Self {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}
		/// Create a `Connection` from a raw owned handle.
		#[inline] pub unsafe fn from_handle_owned(db: *mut ffi::sqlite3) -> Result<Self>
        {
			let db = InnerConnection::new(db, true);
			Ok(Self
            {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}
		/// Get access to a handle that can be used to interrupt long-running queries from another thread.
		#[inline] pub fn get_interrupt_handle(&self) -> InterruptHandle { self.db.borrow().get_interrupt_handle() }

		#[inline] fn decode_result(&self, code: c_int) -> Result<()> { self.db.borrow().decode_result(code) }
		/// Return the number of rows modified, inserted or deleted by the most
		/// recently completed INSERT, UPDATE or DELETE statement on the database
		/// connection.
		#[inline] pub fn changes(&self) -> u64 { self.db.borrow().changes() }
		/// Return the total number of rows modified, inserted or deleted by all
		/// completed INSERT, UPDATE or DELETE statements since the database
		/// connection was opened, including those executed as part of trigger programs.
		#[inline] pub fn total_changes(&self) -> u64 { self.db.borrow().total_changes() }
		/// Test for auto-commit mode.
		#[inline] pub fn is_autocommit(&self) -> bool { self.db.borrow().is_autocommit() }
		/// Determine if all associated prepared statements have been reset.
		#[inline] pub fn is_busy(&self) -> bool { self.db.borrow().is_busy() }
		/// Flush caches to disk mid-transaction
		pub fn cache_flush(&self) -> Result<()> { self.db.borrow_mut().cache_flush() }
		/// Determine if a database is read-only
		pub fn is_readonly<N: Name>(&self, db_name: N) -> Result<bool> { self.db.borrow().db_readonly(db_name) }
		/// Return the schema name for a database connection.
		pub fn db_name(&self, index: usize) -> Result<String>
        {
			unsafe
            {
				let db = self.handle();
				let name = ffi::sqlite3_db_name(db, index as c_int);
				if name.is_null() {
					Err(Error::InvalidDatabaseIndex(index))
				} else {
					Ok(CStr::from_ptr(name).to_str()?.to_owned())
				}
			}
		}
		/// Determine whether an interrupt is currently in effect
		pub fn is_interrupted(&self) -> bool { self.db.borrow().is_interrupted() }
	}

	impl fmt::Debug for Connection
    {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            f.debug_struct("Connection")
            .field("path", &self.path())
            .finish()
		}
	}
	/// Batch fallible iterator
	#[derive(Debug)]
	pub struct Batch<'conn, 'sql>
    {
		conn: &'conn Connection,
		sql: &'sql str,
		tail: usize,
	}

	impl<'conn, 'sql> Batch<'conn, 'sql>
    {
		/// Constructor
		pub fn new(conn: &'conn Connection, sql: &'sql str) -> Self
        { Batch { conn, sql, tail: 0 } }
	}
	impl<'conn> fallible_iterator::FallibleIterator for Batch<'conn, '_>
    {
		type Error = Error;
		type Item = Statement<'conn>;
		/// Iterates on each batch statements.
		fn next(&mut self) -> Result<Option<Statement<'conn>>>
        {
			while self.tail < self.sql.len()
            {
				let sql = &self.sql[self.tail..];
				let (next, tail) = self.conn
                .db
                .borrow_mut()
                .prepare(self.conn, sql, PrepFlags::default())?;

				if tail == 0 {
					self.tail = self.sql.len();
				} else {
					self.tail += tail;
				}
				if next.stmt.is_null() {
					continue;
				}
				return Ok(Some(next));
			}
			Ok(None)
		}
	}

	bitflags::bitflags!
    {
		/// Flags for opening SQLite database connections.
		#[repr(C)] #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
		pub struct OpenFlags: c_int
        {
			/// The database is opened in read-only mode.
			const SQLITE_OPEN_READ_ONLY = ffi::SQLITE_OPEN_READONLY;
			/// The database is opened for reading and writing if possible, or reading only 
            /// if the file is write-protected by the operating system.
			const SQLITE_OPEN_READ_WRITE = ffi::SQLITE_OPEN_READWRITE;
			/// The database is created if it does not already exist
			const SQLITE_OPEN_CREATE = ffi::SQLITE_OPEN_CREATE;
			/// The filename can be interpreted as a URI if this flag is set.
			const SQLITE_OPEN_URI = ffi::SQLITE_OPEN_URI;
			/// The database will be opened as an in-memory database.
			const SQLITE_OPEN_MEMORY = ffi::SQLITE_OPEN_MEMORY;
			/// The new database connection will not use a per-connection mutex via "multi-thread" threading mode
			const SQLITE_OPEN_NO_MUTEX = ffi::SQLITE_OPEN_NOMUTEX;
			/// The new database connection will use a per-connection mutex via "serialized" threading mode
			const SQLITE_OPEN_FULL_MUTEX = ffi::SQLITE_OPEN_FULLMUTEX;
			/// The database is opened with shared cache enabled.
			const SQLITE_OPEN_SHARED_CACHE = 0x0002_0000;
			/// The database is opened shared cache disabled.
			const SQLITE_OPEN_PRIVATE_CACHE = 0x0004_0000;
			/// The database filename is not allowed to be a symbolic link. (3.31.0)
			const SQLITE_OPEN_NOFOLLOW = 0x0100_0000;
			/// Extended result codes. (3.37.0)
			const SQLITE_OPEN_EXRESCODE = 0x0200_0000;
		}
	}

	impl Default for OpenFlags
    {
		#[inline] fn default() -> Self
        {
			Self::SQLITE_OPEN_READ_WRITE
				| Self::SQLITE_OPEN_CREATE
				| Self::SQLITE_OPEN_NO_MUTEX
				| Self::SQLITE_OPEN_URI
		}
	}

	bitflags::bitflags!
    {
		/// Prepare flags.
		#[repr(C)] #[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
		pub struct PrepFlags: c_uint
        {
			/// A hint to the query planner that the prepared statement will be retained for a long time and probably reused many times.
			const SQLITE_PREPARE_PERSISTENT = 0x01;
			/// Causes the SQL compiler to return an error (error code SQLITE_ERROR) if the statement uses any virtual tables.
			const SQLITE_PREPARE_NO_VTAB = 0x04;
			/// Prevents SQL compiler errors from being sent to the error log.
			const SQLITE_PREPARE_DONT_LOG = 0x10;
		}
	}
	/// Allows interrupting a long-running computation.
	pub struct InterruptHandle
    {
		db_lock: Arc<Mutex<*mut ffi::sqlite3>>,
	}

	unsafe impl Send for InterruptHandle {}
	unsafe impl Sync for InterruptHandle {}

	impl InterruptHandle
    {
		/// Interrupt the query currently executing on another thread.
		pub fn interrupt(&self)
        {
			let db_handle = self.db_lock.lock().unwrap();
			if !db_handle.is_null()
            { unsafe { ffi::sqlite3_interrupt(*db_handle) } }
		}
	}
}

pub mod str
{
	pub use std::str::{ * };
}

pub mod sync
{
	pub use std::sync::{ * };
}

pub mod time
{
	/*!
	*/
	use ::
	{
		*,
	};
	/*
	*/
    pub mod lib
    {
        pub use std::time::{ * };
    }
}
// 12727 ////////////////////////////////////////////////////////////////////////////////////////////////////////////
