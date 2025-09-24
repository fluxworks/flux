//! Rusqlite is an ergonomic wrapper for using SQLite from Rust.
/*
#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]

use crate::cache::StatementCache;
use crate::inner_connection::InnerConnection;
use crate::raw_statement::RawStatement;
use crate::types::ValueRef;

pub use crate::bind::BindIndex;
pub use crate::cache::CachedStatement;
#[cfg(feature = "column_decltype")]
pub use crate::column::Column;
#[cfg(feature = "column_metadata")]
pub use crate::column::ColumnMetadata;
pub use crate::error::{to_sqlite_error, Error};
pub use crate::ffi::ErrorCode;
#[cfg(feature = "load_extension")]
pub use crate::load_extension_guard::LoadExtensionGuard;
pub use crate::params::{params_from_iter, Params, ParamsFromIter};
pub use crate::row::{AndThenRows, Map, MappedRows, Row, RowIndex, Rows};
pub use crate::statement::{Statement, StatementStatus};
#[cfg(feature = "modern_sqlite")]
pub use crate::transaction::TransactionState;
pub use crate::transaction::{DropBehavior, Savepoint, Transaction, TransactionBehavior};
pub use crate::types::ToSql;
pub use crate::util::Name;
pub use crate::version::*;
#[cfg(feature = "rusqlite-macros")]
#[doc(hidden)]
pub use rusqlite_macros::__bind;

pub mod _
{
	pub use std::_::{ * };
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

			impl<I, F, B: ?Sized> FallibleStreamingIterator for MapRef<I, F> vwhere
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
			#[inline]
			fn from(e: E) -> FoldStop<T, E> {
				FoldStop::Err(e)
			}
		}

		trait ResultExt<T, E> {
			fn unpack_fold(self) -> Result<T, E>;
		}

		impl<T, E> ResultExt<T, E> for Result<T, FoldStop<T, E>> {
			#[inline]
			fn unpack_fold(self) -> Result<T, E> {
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
			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				(0, None)
			}

			/// Consumes the iterator, returning the number of remaining items.
			#[inline]
			fn count(self) -> Result<usize, Self::Error>
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
			#[inline]
			fn last(self) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
			{
				self.fold(None, |_, v| Ok(Some(v)))
			}

			/// Returns the `n`th element of the iterator.
			#[inline]
			fn nth(&mut self, mut n: usize) -> Result<Option<Self::Item>, Self::Error> {
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
			#[inline]
			fn step_by(self, step: usize) -> StepBy<Self>
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
			#[inline]
			fn chain<I>(self, it: I) -> Chain<Self, I>
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
			#[inline]
			fn zip<I>(self, o: I) -> Zip<Self, I::IntoFallibleIter>
			where
				Self: Sized,
				I: IntoFallibleIterator<Error = Self::Error>,
			{
				Zip(self, o.into_fallible_iter())
			}

			/// Returns an iterator which applies a fallible transform to the elements
			/// of the underlying iterator.
			#[inline]
			fn map<F, B>(self, f: F) -> Map<Self, F>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<B, Self::Error>,
			{
				Map { it: self, f }
			}

			/// Calls a fallible closure on each element of an iterator.
			#[inline]
			fn for_each<F>(self, mut f: F) -> Result<(), Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<(), Self::Error>,
			{
				self.fold((), move |(), item| f(item))
			}

			/// Returns an iterator which uses a predicate to determine which values
			/// should be yielded. The predicate may fail; such failures are passed to
			/// the caller.
			#[inline]
			fn filter<F>(self, f: F) -> Filter<Self, F>
			where
				Self: Sized,
				F: FnMut(&Self::Item) -> Result<bool, Self::Error>,
			{
				Filter { it: self, f }
			}

			/// Returns an iterator which both filters and maps. The closure may fail;
			/// such failures are passed along to the consumer.
			#[inline]
			fn filter_map<B, F>(self, f: F) -> FilterMap<Self, F>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<Option<B>, Self::Error>,
			{
				FilterMap { it: self, f }
			}

			/// Returns an iterator which yields the current iteration count as well
			/// as the value.
			#[inline]
			fn enumerate(self) -> Enumerate<Self>
			where
				Self: Sized,
			{
				Enumerate { it: self, n: 0 }
			}

			/// Returns an iterator that can peek at the next element without consuming
			/// it.
			#[inline]
			fn peekable(self) -> Peekable<Self>
			where
				Self: Sized,
			{
				Peekable {
					it: self,
					next: None,
				}
			}

			/// Returns an iterator that skips elements based on a predicate.
			#[inline]
			fn skip_while<P>(self, predicate: P) -> SkipWhile<Self, P>
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
			#[inline]
			fn take_while<P>(self, predicate: P) -> TakeWhile<Self, P>
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
			#[inline]
			fn skip(self, n: usize) -> Skip<Self>
			where
				Self: Sized,
			{
				Skip { it: self, n }
			}

			/// Returns an iterator that yields only the first `n` values of this
			/// iterator.
			#[inline]
			fn take(self, n: usize) -> Take<Self>
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
			#[inline]
			fn scan<St, B, F>(self, initial_state: St, f: F) -> Scan<Self, St, F>
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
			#[inline]
			fn flat_map<U, F>(self, f: F) -> FlatMap<Self, U, F>
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
			#[inline]
			fn flatten(self) -> Flatten<Self>
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
			#[inline]
			fn fuse(self) -> Fuse<Self>
			where
				Self: Sized,
			{
				Fuse {
					it: self,
					done: false,
				}
			}

			/// Returns an iterator which passes each element to a closure before returning it.
			#[inline]
			fn inspect<F>(self, f: F) -> Inspect<Self, F>
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
			#[inline]
			fn by_ref(&mut self) -> &mut Self
			where
				Self: Sized,
			{
				self
			}

			/// Transforms the iterator into a collection.
			///
			/// An `Err` will be returned if any invocation of `next` returns `Err`.
			#[inline]
			fn collect<T>(self) -> Result<T, Self::Error>
			where
				T: iter::FromIterator<Self::Item>,
				Self: Sized,
			{
				self.iterator().collect()
			}

			/// Transforms the iterator into two collections, partitioning elements by a closure.
			#[inline]
			fn partition<B, F>(self, mut f: F) -> Result<(B, B), Self::Error>
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
			#[inline]
			fn fold<B, F>(mut self, init: B, f: F) -> Result<B, Self::Error>
			where
				Self: Sized,
				F: FnMut(B, Self::Item) -> Result<B, Self::Error>,
			{
				self.try_fold(init, f)
			}

			/// Applies a function over the elements of the iterator, producing a single final value.
			///
			/// This is used as the "base" of many methods on `FallibleIterator`.
			#[inline]
			fn try_fold<B, E, F>(&mut self, mut init: B, mut f: F) -> Result<B, E>
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
			#[inline]
			fn all<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
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
			#[inline]
			fn any<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
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
			#[inline]
			fn find<F>(&mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
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
			#[inline]
			fn find_map<B, F>(&mut self, f: F) -> Result<Option<B>, Self::Error>
			where
				Self: Sized,
				F: FnMut(Self::Item) -> Result<Option<B>, Self::Error>,
			{
				self.filter_map(f).next()
			}

			/// Returns the position of the first element of this iterator that matches
			/// a predicate. The predicate may fail; such failures are returned to the
			/// caller.
			#[inline]
			fn position<F>(&mut self, mut f: F) -> Result<Option<usize>, Self::Error>
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
			#[inline]
			fn max(self) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				Self::Item: Ord,
			{
				self.max_by(|a, b| Ok(a.cmp(b)))
			}

			/// Returns the element of the iterator which gives the maximum value from
			/// the function.
			#[inline]
			fn max_by_key<B, F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
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
			#[inline]
			fn max_by<F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
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
			#[inline]
			fn min(self) -> Result<Option<Self::Item>, Self::Error>
			where
				Self: Sized,
				Self::Item: Ord,
			{
				self.min_by(|a, b| Ok(a.cmp(b)))
			}

			/// Returns the element of the iterator which gives the minimum value from
			/// the function.
			#[inline]
			fn min_by_key<B, F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
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
			#[inline]
			fn min_by<F>(mut self, mut f: F) -> Result<Option<Self::Item>, Self::Error>
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
			#[inline]
			fn rev(self) -> Rev<Self>
			where
				Self: Sized + DoubleEndedFallibleIterator,
			{
				Rev(self)
			}

			/// Converts an iterator of pairs into a pair of containers.
			#[inline]
			fn unzip<A, B, FromA, FromB>(self) -> Result<(FromA, FromB), Self::Error>
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
			#[inline]
			fn cloned<'a, T>(self) -> Cloned<Self>
			where
				Self: Sized + FallibleIterator<Item = &'a T>,
				T: 'a + Clone,
			{
				Cloned(self)
			}

			/// Returns an iterator which repeats this iterator endlessly.
			#[inline]
			fn cycle(self) -> Cycle<Self>
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
			#[inline]
			fn cmp<I>(mut self, other: I) -> Result<Ordering, Self::Error>
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
			#[inline]
			fn partial_cmp<I>(mut self, other: I) -> Result<Option<Ordering>, Self::Error>
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
			#[inline]
			fn eq<I>(mut self, other: I) -> Result<bool, Self::Error>
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
			#[inline]
			fn ne<I>(mut self, other: I) -> Result<bool, Self::Error>
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
			#[inline]
			fn lt<I>(mut self, other: I) -> Result<bool, Self::Error>
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
			#[inline]
			fn le<I>(mut self, other: I) -> Result<bool, Self::Error>
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
			#[inline]
			fn gt<I>(mut self, other: I) -> Result<bool, Self::Error>
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
			#[inline]
			fn ge<I>(mut self, other: I) -> Result<bool, Self::Error>
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
			#[inline]
			fn iterator(self) -> Iterator<Self>
			where
				Self: Sized,
			{
				Iterator(self)
			}

			/// Returns an iterator which applies a transform to the errors of the
			/// underlying iterator.
			#[inline]
			fn map_err<B, F>(self, f: F) -> MapErr<Self, F>
			where
				F: FnMut(Self::Error) -> B,
				Self: Sized,
			{
				MapErr { it: self, f }
			}

			/// Returns an iterator which unwraps all of its elements.
			#[inline]
			fn unwrap<T>(self) -> Unwrap<Self>
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

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next()
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				(**self).size_hint()
			}

			#[inline]
			fn nth(&mut self, n: usize) -> Result<Option<I::Item>, I::Error> {
				(**self).nth(n)
			}
		}

		impl<I: DoubleEndedFallibleIterator + ?Sized> DoubleEndedFallibleIterator for &mut I {
			#[inline]
			fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next_back()
			}
		}

		#[cfg(feature = "alloc")]
		impl<I: FallibleIterator + ?Sized> FallibleIterator for Box<I> {
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next()
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				(**self).size_hint()
			}

			#[inline]
			fn nth(&mut self, n: usize) -> Result<Option<I::Item>, I::Error> {
				(**self).nth(n)
			}
		}

		#[cfg(feature = "alloc")]
		impl<I: DoubleEndedFallibleIterator + ?Sized> DoubleEndedFallibleIterator for Box<I> {
			#[inline]
			fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				(**self).next_back()
			}
		}

		/// A fallible iterator able to yield elements from both ends.
		pub trait DoubleEndedFallibleIterator: FallibleIterator {
			/// Advances the end of the iterator, returning the last value.
			fn next_back(&mut self) -> Result<Option<Self::Item>, Self::Error>;

			/// Applies a function over the elements of the iterator in reverse order, producing a single final value.
			#[inline]
			fn rfold<B, F>(mut self, init: B, f: F) -> Result<B, Self::Error>
			where
				Self: Sized,
				F: FnMut(B, Self::Item) -> Result<B, Self::Error>,
			{
				self.try_rfold(init, f)
			}

			/// Applies a function over the elements of the iterator in reverse, producing a single final value.
			///
			/// This is used as the "base" of many methods on `DoubleEndedFallibleIterator`.
			#[inline]
			fn try_rfold<B, E, F>(&mut self, mut init: B, mut f: F) -> Result<B, E>
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

			#[inline]
			fn into_fallible_iter(self) -> I {
				self
			}
		}

		/// An iterator which applies a fallible transform to the elements of the
		/// underlying iterator.
		#[derive(Clone)]
		pub struct Map<T, F> {
			it: T,
			f: F,
		}

		impl<I: core::fmt::Debug, F> core::fmt::Debug for Map<I, F> {
			fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
				f.debug_struct("Map").field("iter", &self.it).finish()
			}
		}

		impl<T, F, B> FallibleIterator for Map<T, F>
		where
			T: FallibleIterator,
			F: FnMut(T::Item) -> Result<B, T::Error>,
		{
			type Item = B;
			type Error = T::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<B>, T::Error> {
				match self.it.next() {
					Ok(Some(v)) => Ok(Some((self.f)(v)?)),
					Ok(None) => Ok(None),
					Err(e) => Err(e),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline]
			fn try_fold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
			where
				E: From<T::Error>,
				G: FnMut(C, B) -> Result<C, E>,
			{
				let map = &mut self.f;
				self.it.try_fold(init, |b, v| f(b, map(v)?))
			}
		}

		impl<B, F, I> DoubleEndedFallibleIterator for Map<I, F>
		where
			I: DoubleEndedFallibleIterator,
			F: FnMut(I::Item) -> Result<B, I::Error>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<B>, I::Error> {
				match self.it.next_back() {
					Ok(Some(v)) => Ok(Some((self.f)(v)?)),
					Ok(None) => Ok(None),
					Err(e) => Err(e),
				}
			}

			#[inline]
			fn try_rfold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
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

		impl<T, U> FallibleIterator for Chain<T, U>
		where
			T: FallibleIterator,
			U: FallibleIterator<Item = T::Item, Error = T::Error>,
		{
			type Item = T::Item;
			type Error = T::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<T::Item>, T::Error> {
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

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				let front_hint = self.front.size_hint();
				let back_hint = self.back.size_hint();

				let low = front_hint.0.saturating_add(back_hint.0);
				let high = match (front_hint.1, back_hint.1) {
					(Some(f), Some(b)) => f.checked_add(b),
					_ => None,
				};

				(low, high)
			}

			#[inline]
			fn count(self) -> Result<usize, T::Error> {
				match self.state {
					ChainState::Both => Ok(self.front.count()? + self.back.count()?),
					ChainState::Front => self.front.count(),
					ChainState::Back => self.back.count(),
				}
			}

			#[inline]
			fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
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

			#[inline]
			fn find<F>(&mut self, mut f: F) -> Result<Option<T::Item>, T::Error>
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

			#[inline]
			fn last(self) -> Result<Option<T::Item>, T::Error> {
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

		impl<T, U> DoubleEndedFallibleIterator for Chain<T, U>
		where
			T: DoubleEndedFallibleIterator,
			U: DoubleEndedFallibleIterator<Item = T::Item, Error = T::Error>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<T::Item>, T::Error> {
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

			#[inline]
			fn try_rfold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
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

		impl<'a, T, I> FallibleIterator for Cloned<I>
		where
			I: FallibleIterator<Item = &'a T>,
			T: 'a + Clone,
		{
			type Item = T;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<T>, I::Error> {
				self.0.next().map(|o| o.cloned())
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline]
			fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, T) -> Result<B, E>,
			{
				self.0.try_fold(init, |acc, v| f(acc, v.clone()))
			}
		}

		impl<'a, T, I> DoubleEndedFallibleIterator for Cloned<I>
		where
			I: DoubleEndedFallibleIterator<Item = &'a T>,
			T: 'a + Clone,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<T>, I::Error> {
				self.0.next_back().map(|o| o.cloned())
			}

			#[inline]
			fn try_rfold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, T) -> Result<B, E>,
			{
				self.0.try_rfold(init, |acc, v| f(acc, v.clone()))
			}
		}

		/// Converts an `Iterator<Item = Result<T, E>>` into a `FallibleIterator<Item = T, Error = E>`.
		#[inline]
		pub fn convert<T, E, I>(it: I) -> Convert<I>
		where
			I: iter::Iterator<Item = Result<T, E>>,
		{
			Convert(it)
		}

		/// A fallible iterator that wraps a normal iterator over `Result`s.
		#[derive(Clone, Debug)]
		pub struct Convert<I>(I);

		impl<T, E, I> FallibleIterator for Convert<I>
		where
			I: iter::Iterator<Item = Result<T, E>>,
		{
			type Item = T;
			type Error = E;

			#[inline]
			fn next(&mut self) -> Result<Option<T>, E> {
				match self.0.next() {
					Some(Ok(i)) => Ok(Some(i)),
					Some(Err(e)) => Err(e),
					None => Ok(None),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline]
			fn try_fold<B, E2, F>(&mut self, init: B, mut f: F) -> Result<B, E2>
			where
				E2: From<E>,
				F: FnMut(B, T) -> Result<B, E2>,
			{
				self.0.try_fold(init, |acc, v| f(acc, v?))
			}
		}

		impl<T, E, I> DoubleEndedFallibleIterator for Convert<I>
		where
			I: DoubleEndedIterator<Item = Result<T, E>>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<T>, E> {
				match self.0.next_back() {
					Some(Ok(i)) => Ok(Some(i)),
					Some(Err(e)) => Err(e),
					None => Ok(None),
				}
			}

			#[inline]
			fn try_rfold<B, E2, F>(&mut self, init: B, mut f: F) -> Result<B, E2>
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

		impl<T, I> FallibleIterator for IntoFallible<I>
		where
			I: iter::Iterator<Item = T>,
		{
			type Item = T;
			type Error = Infallible;

			#[inline]
			fn next(&mut self) -> Result<Option<T>, Self::Error> {
				Ok(self.0.next())
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline]
			fn try_fold<B, E2, F>(&mut self, init: B, f: F) -> Result<B, E2>
			where
				E2: From<Infallible>,
				F: FnMut(B, T) -> Result<B, E2>,
			{
				self.0.try_fold(init, f)
			}
		}

		impl<T, I: iter::Iterator<Item = T>> From<I> for IntoFallible<I> {
			fn from(value: I) -> Self {
				Self(value)
			}
		}

		impl<T, I> DoubleEndedFallibleIterator for IntoFallible<I>
		where
			I: DoubleEndedIterator<Item = T>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<T>, Infallible> {
				Ok(self.0.next_back())
			}

			#[inline]
			fn try_rfold<B, E2, F>(&mut self, init: B, f: F) -> Result<B, E2>
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

		impl<I> FallibleIterator for Enumerate<I>
		where
			I: FallibleIterator,
		{
			type Item = (usize, I::Item);
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<(usize, I::Item)>, I::Error> {
				self.it.next().map(|o| {
					o.map(|e| {
						let i = self.n;
						self.n += 1;
						(i, e)
					})
				})
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline]
			fn count(self) -> Result<usize, I::Error> {
				self.it.count()
			}

			#[inline]
			fn nth(&mut self, n: usize) -> Result<Option<(usize, I::Item)>, I::Error> {
				match self.it.nth(n)? {
					Some(v) => {
						let i = self.n + n;
						self.n = i + 1;
						Ok(Some((i, v)))
					}
					None => Ok(None),
				}
			}

			#[inline]
			fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
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

		impl<I, F> FallibleIterator for Filter<I, F>
		where
			I: FallibleIterator,
			F: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
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

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.it.size_hint().1)
			}

			#[inline]
			fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
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

		impl<I, F> DoubleEndedFallibleIterator for Filter<I, F>
		where
			I: DoubleEndedFallibleIterator,
			F: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
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

			#[inline]
			fn try_rfold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
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

		impl<B, I, F> FallibleIterator for FilterMap<I, F>
		where
			I: FallibleIterator,
			F: FnMut(I::Item) -> Result<Option<B>, I::Error>,
		{
			type Item = B;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<B>, I::Error> {
				let map = &mut self.f;
				self.it
					.try_fold((), |(), v| match map(v)? {
						Some(v) => Err(FoldStop::Break(Some(v))),
						None => Ok(()),
					})
					.map(|()| None)
					.unpack_fold()
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				(0, self.it.size_hint().1)
			}

			#[inline]
			fn try_fold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
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

		impl<B, I, F> DoubleEndedFallibleIterator for FilterMap<I, F>
		where
			I: DoubleEndedFallibleIterator,
			F: FnMut(I::Item) -> Result<Option<B>, I::Error>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<B>, I::Error> {
				let map = &mut self.f;
				self.it
					.try_rfold((), |(), v| match map(v)? {
						Some(v) => Err(FoldStop::Break(Some(v))),
						None => Ok(()),
					})
					.map(|()| None)
					.unpack_fold()
			}

			#[inline]
			fn try_rfold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
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
		pub struct FlatMap<I, U, F>
		where
			U: IntoFallibleIterator,
		{
			it: Map<I, F>,
			cur: Option<U::IntoFallibleIter>,
		}

		impl<I, U, F> FallibleIterator for FlatMap<I, U, F>
		where
			I: FallibleIterator,
			U: IntoFallibleIterator<Error = I::Error>,
			F: FnMut(I::Item) -> Result<U, I::Error>,
		{
			type Item = U::Item;
			type Error = U::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<U::Item>, U::Error> {
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

			#[inline]
			fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
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
		pub struct Flatten<I>
		where
			I: FallibleIterator,
			I::Item: IntoFallibleIterator,
		{
			it: I,
			cur: Option<<I::Item as IntoFallibleIterator>::IntoFallibleIter>,
		}

		impl<I> Clone for Flatten<I>
		where
			I: FallibleIterator + Clone,
			I::Item: IntoFallibleIterator,
			<I::Item as IntoFallibleIterator>::IntoFallibleIter: Clone,
		{
			#[inline]
			fn clone(&self) -> Flatten<I> {
				Flatten {
					it: self.it.clone(),
					cur: self.cur.clone(),
				}
			}
		}

		impl<I> FallibleIterator for Flatten<I>
		where
			I: FallibleIterator,
			I::Item: IntoFallibleIterator<Error = I::Error>,
		{
			type Item = <I::Item as IntoFallibleIterator>::Item;
			type Error = <I::Item as IntoFallibleIterator>::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
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

			#[inline]
			fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
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
		///
		/// ```
		/// # use fallible_iterator::{from_fn, FallibleIterator};
		/// let mut count = 0;
		/// let counter = from_fn(move || {
		///     // Increment our count. This is why we started at zero.
		///     count += 1;
		///
		///     // Check to see if we've finished counting or not.
		///     if count < 6 {
		///         Ok(Some(count))
		///     } else if count < 7 {
		///         Ok(None)
		///     } else {
		///         Err(())
		///     }
		/// });
		/// assert_eq!(&counter.collect::<Vec<_>>().unwrap(), &[1, 2, 3, 4, 5]);
		/// ```
		#[inline]
		pub fn from_fn<I, E, F>(fun: F) -> FromFn<F>
		where
			F: FnMut() -> Result<Option<I>, E>,
		{
			FromFn { fun }
		}

		/// An iterator using a function to generate new values.
		#[derive(Clone, Debug)]
		pub struct FromFn<F> {
			fun: F,
		}

		impl<I, E, F> FallibleIterator for FromFn<F>
		where
			F: FnMut() -> Result<Option<I>, E>,
		{
			type Item = I;
			type Error = E;

			fn next(&mut self) -> Result<Option<I>, E> {
				(self.fun)()
			}
		}

		/// An iterator that yields `Ok(None)` forever after the underlying iterator
		/// yields `Ok(None)` once.
		#[derive(Clone, Debug)]
		pub struct Fuse<I> {
			it: I,
			done: bool,
		}

		impl<I> FallibleIterator for Fuse<I>
		where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
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

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				if self.done {
					(0, Some(0))
				} else {
					self.it.size_hint()
				}
			}

			#[inline]
			fn count(self) -> Result<usize, I::Error> {
				if self.done {
					Ok(0)
				} else {
					self.it.count()
				}
			}

			#[inline]
			fn last(self) -> Result<Option<I::Item>, I::Error> {
				if self.done {
					Ok(None)
				} else {
					self.it.last()
				}
			}

			#[inline]
			fn nth(&mut self, n: usize) -> Result<Option<I::Item>, I::Error> {
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

			#[inline]
			fn try_fold<B, E, F>(&mut self, init: B, f: F) -> Result<B, E>
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

		impl<I, F> FallibleIterator for Inspect<I, F>
		where
			I: FallibleIterator,
			F: FnMut(&I::Item) -> Result<(), I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				match self.it.next()? {
					Some(i) => {
						(self.f)(&i)?;
						Ok(Some(i))
					}
					None => Ok(None),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline]
			fn try_fold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
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

		impl<I, F> DoubleEndedFallibleIterator for Inspect<I, F>
		where
			I: DoubleEndedFallibleIterator,
			F: FnMut(&I::Item) -> Result<(), I::Error>,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				match self.it.next_back()? {
					Some(i) => {
						(self.f)(&i)?;
						Ok(Some(i))
					}
					None => Ok(None),
				}
			}

			#[inline]
			fn try_rfold<B, E, G>(&mut self, init: B, mut f: G) -> Result<B, E>
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

		impl<I> iter::Iterator for Iterator<I>
		where
			I: FallibleIterator,
		{
			type Item = Result<I::Item, I::Error>;

			#[inline]
			fn next(&mut self) -> Option<Result<I::Item, I::Error>> {
				match self.0.next() {
					Ok(Some(v)) => Some(Ok(v)),
					Ok(None) => None,
					Err(e) => Some(Err(e)),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}
		}

		impl<I> DoubleEndedIterator for Iterator<I>
		where
			I: DoubleEndedFallibleIterator,
		{
			#[inline]
			fn next_back(&mut self) -> Option<Result<I::Item, I::Error>> {
				match self.0.next_back() {
					Ok(Some(v)) => Some(Ok(v)),
					Ok(None) => None,
					Err(e) => Some(Err(e)),
				}
			}
		}

		/// An iterator which applies a transform to the errors of the underlying
		/// iterator.
		#[derive(Clone, Debug)]
		pub struct MapErr<I, F> {
			it: I,
			f: F,
		}

		impl<B, F, I> FallibleIterator for MapErr<I, F>
		where
			I: FallibleIterator,
			F: FnMut(I::Error) -> B,
		{
			type Item = I::Item;
			type Error = B;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, B> {
				self.it.next().map_err(&mut self.f)
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.it.size_hint()
			}

			#[inline]
			fn count(mut self) -> Result<usize, B> {
				self.it.count().map_err(&mut self.f)
			}

			#[inline]
			fn last(mut self) -> Result<Option<I::Item>, B> {
				self.it.last().map_err(&mut self.f)
			}

			#[inline]
			fn nth(&mut self, n: usize) -> Result<Option<I::Item>, B> {
				self.it.nth(n).map_err(&mut self.f)
			}

			#[inline]
			fn try_fold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
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

		impl<B, F, I> DoubleEndedFallibleIterator for MapErr<I, F>
		where
			I: DoubleEndedFallibleIterator,
			F: FnMut(I::Error) -> B,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<I::Item>, B> {
				self.it.next_back().map_err(&mut self.f)
			}

			#[inline]
			fn try_rfold<C, E, G>(&mut self, init: C, mut f: G) -> Result<C, E>
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
			#[inline]
			fn from(t: T) -> MappedErr<T, U> {
				MappedErr::It(t)
			}
		}

		/// An iterator which can look at the next element without consuming it.
		#[derive(Clone, Debug)]
		pub struct Peekable<I: FallibleIterator> {
			it: I,
			next: Option<I::Item>,
		}

		impl<I> Peekable<I>
		where
			I: FallibleIterator,
		{
			/// Returns a reference to the next value without advancing the iterator.
			#[inline]
			pub fn peek(&mut self) -> Result<Option<&I::Item>, I::Error> {
				if self.next.is_none() {
					self.next = self.it.next()?;
				}

				Ok(self.next.as_ref())
			}

			/// Consume and return the next value of this iterator if a condition is true.
			///
			/// If func returns true for the next value of this iterator, consume and return it. Otherwise, return None.
			#[inline]
			pub fn next_if(&mut self, f: impl Fn(&I::Item) -> bool) -> Result<Option<I::Item>, I::Error> {
				match self.peek()? {
					Some(item) if f(item) => self.next(),
					_ => Ok(None),
				}
			}

			/// Consume and return the next item if it is equal to `expected`.
			#[inline]
			pub fn next_if_eq<T>(&mut self, expected: &T) -> Result<Option<I::Item>, I::Error>
			where
				T: ?Sized,
				I::Item: PartialEq<T>,
			{
				self.next_if(|found| found == expected)
			}
		}

		impl<I> FallibleIterator for Peekable<I>
		where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if let Some(next) = self.next.take() {
					return Ok(Some(next));
				}

				self.it.next()
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				let mut hint = self.it.size_hint();
				if self.next.is_some() {
					hint.0 = hint.0.saturating_add(1);
					hint.1 = hint.1.and_then(|h| h.checked_add(1));
				}
				hint
			}

			#[inline]
			fn try_fold<B, E, F>(&mut self, init: B, mut f: F) -> Result<B, E>
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

		impl<I> FallibleIterator for Rev<I>
		where
			I: DoubleEndedFallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				self.0.next_back()
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				self.0.size_hint()
			}

			#[inline]
			fn count(self) -> Result<usize, I::Error> {
				self.0.count()
			}

			#[inline]
			fn try_fold<B, E, F>(&mut self, init: B, f: F) -> Result<B, E>
			where
				E: From<I::Error>,
				F: FnMut(B, I::Item) -> Result<B, E>,
			{
				self.0.try_rfold(init, f)
			}
		}

		impl<I> DoubleEndedFallibleIterator for Rev<I>
		where
			I: DoubleEndedFallibleIterator,
		{
			#[inline]
			fn next_back(&mut self) -> Result<Option<I::Item>, I::Error> {
				self.0.next()
			}

			#[inline]
			fn try_rfold<B, E, F>(&mut self, init: B, f: F) -> Result<B, E>
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

		impl<B, I, St, F> FallibleIterator for Scan<I, St, F>
		where
			I: FallibleIterator,
			F: FnMut(&mut St, I::Item) -> Result<Option<B>, I::Error>,
		{
			type Item = B;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<B>, I::Error> {
				match self.it.next()? {
					Some(v) => (self.f)(&mut self.state, v),
					None => Ok(None),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<I> FallibleIterator for Skip<I>
		where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.n == 0 {
					self.it.next()
				} else {
					let n = self.n;
					self.n = 0;
					self.it.nth(n)
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<I, P> FallibleIterator for SkipWhile<I, P>
		where
			I: FallibleIterator,
			P: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
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

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<I> FallibleIterator for StepBy<I>
		where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
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

		impl<I> FallibleIterator for Take<I>
		where
			I: FallibleIterator,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				if self.remaining == 0 {
					return Ok(None);
				}

				let next = self.it.next();
				if let Ok(Some(_)) = next {
					self.remaining -= 1;
				}
				next
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<I, P> FallibleIterator for TakeWhile<I, P>
		where
			I: FallibleIterator,
			P: FnMut(&I::Item) -> Result<bool, I::Error>,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
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

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<I> FallibleIterator for Cycle<I>
		where
			I: FallibleIterator + Clone,
		{
			type Item = I::Item;
			type Error = I::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<I::Item>, I::Error> {
				match self.cur.next()? {
					None => {
						self.cur = self.it.clone();
						self.cur.next()
					}
					Some(v) => Ok(Some(v)),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				(usize::max_value(), None)
			}
		}

		/// An iterator that yields pairs of this iterator's and another iterator's
		/// values.
		#[derive(Clone, Debug)]
		pub struct Zip<T, U>(T, U);

		impl<T, U> FallibleIterator for Zip<T, U>
		where
			T: FallibleIterator,
			U: FallibleIterator<Error = T::Error>,
		{
			type Item = (T::Item, U::Item);
			type Error = T::Error;

			#[inline]
			fn next(&mut self) -> Result<Option<(T::Item, U::Item)>, T::Error> {
				match (self.0.next()?, self.1.next()?) {
					(Some(a), Some(b)) => Ok(Some((a, b))),
					_ => Ok(None),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<T> iter::Iterator for Unwrap<T>
		where
			T: FallibleIterator,
			T::Error: core::fmt::Debug,
		{
			type Item = T::Item;

			#[inline]
			fn next(&mut self) -> Option<T::Item> {
				self.0.next().unwrap()
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
				let (_, max) = self.0.size_hint();
				(0, max)
			}
		}

		impl<T> iter::DoubleEndedIterator for Unwrap<T>
		where
			T: DoubleEndedFallibleIterator,
			T::Error: core::fmt::Debug,
		{
			#[inline]
			fn next_back(&mut self) -> Option<T::Item> {
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

			#[inline]
			fn next(&mut self) -> Result<Option<T>, E> {
				Ok(None)
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

			#[inline]
			fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				Ok(self.0.take())
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

			#[inline]
			fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				match self.1.take() {
					Some(value) => Err(value),
					None => Ok(None),
				}
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

			#[inline]
			fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				Ok(Some(self.0.clone()))
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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

		impl<T, E: Clone> FallibleIterator for RepeatErr<T, E> {
			type Item = T;
			type Error = E;

			#[inline]
			fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
				Err(self.1.clone())
			}

			#[inline]
			fn size_hint(&self) -> (usize, Option<usize>) {
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
	pub mod error
	{
		/*!
		*/
		use ::
		{
			os::raw::c_int,
			*,
		};
		/*
		use crate::types::FromSqlError;
		use crate::types::Type;
		use crate::{errmsg_to_string, ffi, Result};
		use std::error;
		use std::ffi::{c_char, c_int, NulError};
		use std::fmt;
		use std::path::
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
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				match *self {
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
					unsafe extern "C" fn(
						db: *mut sqlite3,
						pzErrMsg: *mut *mut ::os::raw::c_char,
						_: *const sqlite3_api_routines,
					) -> ::os::raw::c_int,
				>,
			) -> ::os::raw::c_int;
			
			pub fn sqlite3_cancel_auto_extension
			(
				xEntryPoint: ::option::Option<
					unsafe extern "C" fn(
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
						arg2: *const ::os::raw::c_char,
					),
				>,
				arg2: *mut ::os::raw::c_void,
			) -> *mut ::os::raw::c_void;
			pub fn sqlite3_profile(
				arg1: *mut sqlite3,
				xProfile: ::option::Option<
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: ::os::raw::c_uint,
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
					unsafe extern "C" fn(
						arg1: *mut sqlite3_context,
						arg2: ::os::raw::c_int,
						arg3: *mut *mut sqlite3_value,
					),
				>,
				xStep: ::option::Option<
					unsafe extern "C" fn(
						arg1: *mut sqlite3_context,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						apArg: *mut *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						pCtx: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						arg1: *mut sqlite3_rtree_geometry,
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
					unsafe extern "C" fn(
						pCtx: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						pCtx: *mut ::os::raw::c_void,
						zTab: *const ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				xConflict: ::option::Option<
					unsafe extern "C" fn(
						pCtx: *mut ::os::raw::c_void,
						eConflict: ::os::raw::c_int,
						p: *mut sqlite3_changeset_iter,
					) -> ::os::raw::c_int,
				>,
				pCtx: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_apply_strm(
				db: *mut sqlite3,
				xInput: ::option::Option<
					unsafe extern "C" fn(
						pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
				xFilter: ::option::Option<
					unsafe extern "C" fn(
						pCtx: *mut ::os::raw::c_void,
						zTab: *const ::os::raw::c_char,
					) -> ::os::raw::c_int,
				>,
				xConflict: ::option::Option<
					unsafe extern "C" fn(
						pCtx: *mut ::os::raw::c_void,
						eConflict: ::os::raw::c_int,
						p: *mut sqlite3_changeset_iter,
					) -> ::os::raw::c_int,
				>,
				pCtx: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_concat_strm(
				xInputA: ::option::Option<
					unsafe extern "C" fn(
						pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pInA: *mut ::os::raw::c_void,
				xInputB: ::option::Option<
					unsafe extern "C" fn(
						pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pInB: *mut ::os::raw::c_void,
				xOutput: ::option::Option<
					unsafe extern "C" fn(
						pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_invert_strm(
				xInput: ::option::Option<
					unsafe extern "C" fn(
						pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
				xOutput: ::option::Option<
					unsafe extern "C" fn(
						pOut: *mut ::os::raw::c_void,
						pData: *const ::os::raw::c_void,
						nData: ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pOut: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3changeset_start_strm(
				pp: *mut *mut sqlite3_changeset_iter,
				xInput: ::option::Option<
					unsafe extern "C" fn(
						pIn: *mut ::os::raw::c_void,
						pData: *mut ::os::raw::c_void,
						pnData: *mut ::os::raw::c_int,
					) -> ::os::raw::c_int,
				>,
				pIn: *mut ::os::raw::c_void,
			) -> ::os::raw::c_int;
			pub fn sqlite3session_changeset_strm(
				pSession: *mut sqlite3_session,
				xOutput: ::option::Option<
					unsafe extern "C" fn(
						pOut: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						pOut: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						pIn: *mut ::os::raw::c_void,
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
					unsafe extern "C" fn(
						pOut: *mut ::os::raw::c_void,
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
			pub xClose: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::os::raw::c_int,
			>,
			pub xRead: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					arg2: *mut ::os::raw::c_void,
					iAmt: ::os::raw::c_int,
					iOfst: sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xWrite: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					arg2: *const ::os::raw::c_void,
					iAmt: ::os::raw::c_int,
					iOfst: sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xTruncate: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_file, size: sqlite3_int64) -> ::os::raw::c_int,
			>,
			pub xSync: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					flags: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFileSize: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					pSize: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xLock: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					arg2: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xUnlock: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					arg2: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xCheckReservedLock: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					pResOut: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFileControl: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					op: ::os::raw::c_int,
					pArg: *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xSectorSize: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::os::raw::c_int,
			>,
			pub xDeviceCharacteristics: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_file) -> ::os::raw::c_int,
			>,
			pub xShmMap: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					iPg: ::os::raw::c_int,
					pgsz: ::os::raw::c_int,
					arg2: ::os::raw::c_int,
					arg3: *mut *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xShmLock: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					offset: ::os::raw::c_int,
					n: ::os::raw::c_int,
					flags: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xShmBarrier: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_file)>,
			pub xShmUnmap: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					deleteFlag: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFetch: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
					iOfst: sqlite3_int64,
					iAmt: ::os::raw::c_int,
					pp: *mut *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xUnfetch: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_file,
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
			pub xOpen: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					arg2: *mut sqlite3_file,
					flags: ::os::raw::c_int,
					pOutFlags: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xDelete: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					syncDir: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xAccess: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					flags: ::os::raw::c_int,
					pResOut: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xFullPathname: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					nOut: ::os::raw::c_int,
					zOut: *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xDlOpen: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zFilename: *const ::os::raw::c_char,
				) -> *mut ::os::raw::c_void,
			>,
			pub xDlError: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					nByte: ::os::raw::c_int,
					zErrMsg: *mut ::os::raw::c_char,
				),
			>,
			pub xDlSym: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					arg2: *mut ::os::raw::c_void,
					zSymbol: *const ::os::raw::c_char,
				) -> ::option::Option<
					unsafe extern "C" fn(
						arg1: *mut sqlite3_vfs,
						arg2: *mut ::os::raw::c_void,
						zSymbol: *const ::os::raw::c_char,
					),
				>,
			>,
			pub xDlClose: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_vfs, arg2: *mut ::os::raw::c_void),
			>,
			pub xRandomness: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					nByte: ::os::raw::c_int,
					zOut: *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xSleep: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					microseconds: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xCurrentTime: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_vfs, arg2: *mut f64) -> ::os::raw::c_int,
			>,
			pub xGetLastError: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					arg2: ::os::raw::c_int,
					arg3: *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xCurrentTimeInt64: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					arg2: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xSetSystemCall: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
					arg2: sqlite3_syscall_ptr,
				) -> ::os::raw::c_int,
			>,
			pub xGetSystemCall: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
				) -> sqlite3_syscall_ptr,
			>,
			pub xNextSystemCall: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vfs,
					zName: *const ::os::raw::c_char,
				) -> *const ::os::raw::c_char,
			>,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_mem_methods
		{
			pub xMalloc: ::option::Option<
				unsafe extern "C" fn(arg1: ::os::raw::c_int) -> *mut ::os::raw::c_void,
			>,
			pub xFree: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub xRealloc: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut ::os::raw::c_void,
					arg2: ::os::raw::c_int,
				) -> *mut ::os::raw::c_void,
			>,
			pub xSize: ::option::Option<
				unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xRoundup: ::option::Option<
				unsafe extern "C" fn(arg1: ::os::raw::c_int) -> ::os::raw::c_int,
			>,
			pub xInit: ::option::Option<
				unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xShutdown: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub pAppData: *mut ::os::raw::c_void,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_stmt
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
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
			pub xCreate: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3,
					pAux: *mut ::os::raw::c_void,
					argc: ::os::raw::c_int,
					argv: *const *const ::os::raw::c_char,
					ppVTab: *mut *mut sqlite3_vtab,
					arg2: *mut *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xConnect: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3,
					pAux: *mut ::os::raw::c_void,
					argc: ::os::raw::c_int,
					argv: *const *const ::os::raw::c_char,
					ppVTab: *mut *mut sqlite3_vtab,
					arg2: *mut *mut ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xBestIndex: ::option::Option<
				unsafe extern "C" fn(
					pVTab: *mut sqlite3_vtab,
					arg1: *mut sqlite3_index_info,
				) -> ::os::raw::c_int,
			>,
			pub xDisconnect: ::option::Option<
				unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xDestroy: ::option::Option<
				unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xOpen: ::option::Option<
				unsafe extern "C" fn(
					pVTab: *mut sqlite3_vtab,
					ppCursor: *mut *mut sqlite3_vtab_cursor,
				) -> ::os::raw::c_int,
			>,
			pub xClose: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::os::raw::c_int,
			>,
			pub xFilter: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vtab_cursor,
					idxNum: ::os::raw::c_int,
					idxStr: *const ::os::raw::c_char,
					argc: ::os::raw::c_int,
					argv: *mut *mut sqlite3_value,
				) -> ::os::raw::c_int,
			>,
			pub xNext: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::os::raw::c_int,
			>,
			pub xEof: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_vtab_cursor) -> ::os::raw::c_int,
			>,
			pub xColumn: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vtab_cursor,
					arg2: *mut sqlite3_context,
					arg3: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRowid: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vtab_cursor,
					pRowid: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xUpdate: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_vtab,
					arg2: ::os::raw::c_int,
					arg3: *mut *mut sqlite3_value,
					arg4: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xBegin: ::option::Option<
				unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xSync: ::option::Option<
				unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xCommit: ::option::Option<
				unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xRollback: ::option::Option<
				unsafe extern "C" fn(pVTab: *mut sqlite3_vtab) -> ::os::raw::c_int,
			>,
			pub xFindFunction: ::option::Option<
				unsafe extern "C" fn(
					pVtab: *mut sqlite3_vtab,
					nArg: ::os::raw::c_int,
					zName: *const ::os::raw::c_char,
					pxFunc: *mut ::option::Option<
						unsafe extern "C" fn(
							arg1: *mut sqlite3_context,
							arg2: ::os::raw::c_int,
							arg3: *mut *mut sqlite3_value,
						),
					>,
					ppArg: *mut *mut ::os::raw::c_void,
				) -> ::os::raw::c_int,
			>,
			pub xRename: ::option::Option<
				unsafe extern "C" fn(
					pVtab: *mut sqlite3_vtab,
					zNew: *const ::os::raw::c_char,
				) -> ::os::raw::c_int,
			>,
			pub xSavepoint: ::option::Option<
				unsafe extern "C" fn(
					pVTab: *mut sqlite3_vtab,
					arg1: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRelease: ::option::Option<
				unsafe extern "C" fn(
					pVTab: *mut sqlite3_vtab,
					arg1: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRollbackTo: ::option::Option<
				unsafe extern "C" fn(
					pVTab: *mut sqlite3_vtab,
					arg1: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
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
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_constraint
		{
			pub iColumn: ::os::raw::c_int,
			pub op: ::os::raw::c_uchar,
			pub usable: ::os::raw::c_uchar,
			pub iTermOffset: ::os::raw::c_int,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_orderby
		{
			pub iColumn: ::os::raw::c_int,
			pub desc: ::os::raw::c_uchar,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_index_constraint_usage
		{
			pub argvIndex: ::os::raw::c_int,
			pub omit: ::os::raw::c_uchar,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_vtab
		{
			pub pModule: *const sqlite3_module,
			pub nRef: ::os::raw::c_int,
			pub zErrMsg: *mut ::os::raw::c_char,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_vtab_cursor
		{
			pub pVtab: *mut sqlite3_vtab,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_blob
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_mutex_methods
		{
			pub xMutexInit: ::option::Option<unsafe extern "C" fn() -> ::os::raw::c_int>,
			pub xMutexEnd: ::option::Option<unsafe extern "C" fn() -> ::os::raw::c_int>,
			pub xMutexAlloc: ::option::Option<
				unsafe extern "C" fn(arg1: ::os::raw::c_int) -> *mut sqlite3_mutex,
			>,
			pub xMutexFree: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
			pub xMutexEnter: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
			pub xMutexTry: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int,
			>,
			pub xMutexLeave: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_mutex)>,
			pub xMutexHeld: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int,
			>,
			pub xMutexNotheld: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_mutex) -> ::os::raw::c_int,
			>,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache_page
		{
			pub pBuf: *mut ::os::raw::c_void,
			pub pExtra: *mut ::os::raw::c_void,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache_methods2
		{
			pub iVersion: ::os::raw::c_int,
			pub pArg: *mut ::os::raw::c_void,
			pub xInit: ::option::Option<
				unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xShutdown: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub xCreate: ::option::Option<
				unsafe extern "C" fn(
					szPage: ::os::raw::c_int,
					szExtra: ::os::raw::c_int,
					bPurgeable: ::os::raw::c_int,
				) -> *mut sqlite3_pcache,
			>,
			pub xCachesize: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_pcache, nCachesize: ::os::raw::c_int),
			>,
			pub xPagecount: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_pcache) -> ::os::raw::c_int,
			>,
			pub xFetch: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_pcache,
					key: ::os::raw::c_uint,
					createFlag: ::os::raw::c_int,
				) -> *mut sqlite3_pcache_page,
			>,
			pub xUnpin: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_pcache,
					arg2: *mut sqlite3_pcache_page,
					discard: ::os::raw::c_int,
				),
			>,
			pub xRekey: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_pcache,
					arg2: *mut sqlite3_pcache_page,
					oldKey: ::os::raw::c_uint,
					newKey: ::os::raw::c_uint,
				),
			>,
			pub xTruncate: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_pcache, iLimit: ::os::raw::c_uint),
			>,
			pub xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
			pub xShrink: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_pcache_methods
		{
			pub pArg: *mut ::os::raw::c_void,
			pub xInit: ::option::Option<
				unsafe extern "C" fn(arg1: *mut ::os::raw::c_void) -> ::os::raw::c_int,
			>,
			pub xShutdown: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
			pub xCreate: ::option::Option<
				unsafe extern "C" fn(
					szPage: ::os::raw::c_int,
					bPurgeable: ::os::raw::c_int,
				) -> *mut sqlite3_pcache,
			>,
			pub xCachesize: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_pcache, nCachesize: ::os::raw::c_int),
			>,
			pub xPagecount: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_pcache) -> ::os::raw::c_int,
			>,
			pub xFetch: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_pcache,
					key: ::os::raw::c_uint,
					createFlag: ::os::raw::c_int,
				) -> *mut ::os::raw::c_void,
			>,
			pub xUnpin: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_pcache,
					arg2: *mut ::os::raw::c_void,
					discard: ::os::raw::c_int,
				),
			>,
			pub xRekey: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut sqlite3_pcache,
					arg2: *mut ::os::raw::c_void,
					oldKey: ::os::raw::c_uint,
					newKey: ::os::raw::c_uint,
				),
			>,
			pub xTruncate: ::option::Option<
				unsafe extern "C" fn(arg1: *mut sqlite3_pcache, iLimit: ::os::raw::c_uint),
			>,
			pub xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut sqlite3_pcache)>,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_backup
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_snapshot
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_rtree_geometry
		{
			pub pContext: *mut ::os::raw::c_void,
			pub nParam: ::os::raw::c_int,
			pub aParam: *mut sqlite3_rtree_dbl,
			pub pUser: *mut ::os::raw::c_void,
			pub xDelUser: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
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
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_session
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_changeset_iter
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct sqlite3_changegroup
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct Fts5Context
		{
			_unused: [u8; 0],
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct Fts5PhraseIter
		{
			pub a: *const ::os::raw::c_uchar,
			pub b: *const ::os::raw::c_uchar,
		}
		#[repr(C)] #[derive(Debug, Copy, Clone)]
		pub struct Fts5ExtensionApi
		{
			pub iVersion: ::os::raw::c_int,
			pub xUserData: ::option::Option<
				unsafe extern "C" fn(arg1: *mut Fts5Context) -> *mut ::os::raw::c_void,
			>,
			pub xColumnCount: ::option::Option<
				unsafe extern "C" fn(arg1: *mut Fts5Context) -> ::os::raw::c_int,
			>,
			pub xRowCount: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					pnRow: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xColumnTotalSize: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iCol: ::os::raw::c_int,
					pnToken: *mut sqlite3_int64,
				) -> ::os::raw::c_int,
			>,
			pub xTokenize: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					pText: *const ::os::raw::c_char,
					nText: ::os::raw::c_int,
					pCtx: *mut ::os::raw::c_void,
					xToken: ::option::Option<
						unsafe extern "C" fn(
							arg1: *mut ::os::raw::c_void,
							arg2: ::os::raw::c_int,
							arg3: *const ::os::raw::c_char,
							arg4: ::os::raw::c_int,
							arg5: ::os::raw::c_int,
							arg6: ::os::raw::c_int,
						) -> ::os::raw::c_int,
					>,
				) -> ::os::raw::c_int,
			>,
			pub xPhraseCount: ::option::Option<
				unsafe extern "C" fn(arg1: *mut Fts5Context) -> ::os::raw::c_int,
			>,
			pub xPhraseSize: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xInstCount: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					pnInst: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xInst: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iIdx: ::os::raw::c_int,
					piPhrase: *mut ::os::raw::c_int,
					piCol: *mut ::os::raw::c_int,
					piOff: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xRowid:
				::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Context) -> sqlite3_int64>,
			pub xColumnText: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iCol: ::os::raw::c_int,
					pz: *mut *const ::os::raw::c_char,
					pn: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xColumnSize: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iCol: ::os::raw::c_int,
					pnToken: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xQueryPhrase: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
					pUserData: *mut ::os::raw::c_void,
					arg2: ::option::Option<
						unsafe extern "C" fn(
							arg1: *const Fts5ExtensionApi,
							arg2: *mut Fts5Context,
							arg3: *mut ::os::raw::c_void,
						) -> ::os::raw::c_int,
					>,
				) -> ::os::raw::c_int,
			>,
			pub xSetAuxdata: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					pAux: *mut ::os::raw::c_void,
					xDelete: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				) -> ::os::raw::c_int,
			>,
			pub xGetAuxdata: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					bClear: ::os::raw::c_int,
				) -> *mut ::os::raw::c_void,
			>,
			pub xPhraseFirst: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
					arg2: *mut Fts5PhraseIter,
					arg3: *mut ::os::raw::c_int,
					arg4: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xPhraseNext: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					arg2: *mut Fts5PhraseIter,
					piCol: *mut ::os::raw::c_int,
					piOff: *mut ::os::raw::c_int,
				),
			>,
			pub xPhraseFirstColumn: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
					iPhrase: ::os::raw::c_int,
					arg2: *mut Fts5PhraseIter,
					arg3: *mut ::os::raw::c_int,
				) -> ::os::raw::c_int,
			>,
			pub xPhraseNextColumn: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Context,
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
			pub xCreate: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut ::os::raw::c_void,
					azArg: *mut *const ::os::raw::c_char,
					nArg: ::os::raw::c_int,
					ppOut: *mut *mut Fts5Tokenizer,
				) -> ::os::raw::c_int,
			>,
			pub xDelete: ::option::Option<unsafe extern "C" fn(arg1: *mut Fts5Tokenizer)>,
			pub xTokenize: ::option::Option<
				unsafe extern "C" fn(
					arg1: *mut Fts5Tokenizer,
					pCtx: *mut ::os::raw::c_void,
					flags: ::os::raw::c_int,
					pText: *const ::os::raw::c_char,
					nText: ::os::raw::c_int,
					xToken: ::option::Option<
						unsafe extern "C" fn(
							pCtx: *mut ::os::raw::c_void,
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
			pub xCreateTokenizer: ::option::Option<
				unsafe extern "C" fn(
					pApi: *mut fts5_api,
					zName: *const ::os::raw::c_char,
					pContext: *mut ::os::raw::c_void,
					pTokenizer: *mut fts5_tokenizer,
					xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				) -> ::os::raw::c_int,
			>,
			pub xFindTokenizer: ::option::Option<
				unsafe extern "C" fn(
					pApi: *mut fts5_api,
					zName: *const ::os::raw::c_char,
					ppContext: *mut *mut ::os::raw::c_void,
					pTokenizer: *mut fts5_tokenizer,
				) -> ::os::raw::c_int,
			>,
			pub xCreateFunction: ::option::Option<
				unsafe extern "C" fn(
					pApi: *mut fts5_api,
					zName: *const ::os::raw::c_char,
					pContext: *mut ::os::raw::c_void,
					xFunction: fts5_extension_function,
					xDestroy: ::option::Option<unsafe extern "C" fn(arg1: *mut ::os::raw::c_void)>,
				) -> ::os::raw::c_int,
			>,
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
	
	mod bind;
	mod busy;
	mod cache;
	mod column;
	pub mod config;
	mod inner_connection;
	mod params;
	mod pragma;
	mod raw_statement;
	mod row;
	mod statement;
	mod transaction;
	pub mod types;
	mod version;

	pub(crate) mod util;

	// Actually, only sqlite3_enable_load_extension is disabled (not sqlite3_load_extension)
	#[cfg(all(feature = "loadable_extension", feature = "load_extension"))]
	compile_error!("feature \"loadable_extension\" and feature \"load_extension\" cannot be enabled at the same time");

	// Number of cached prepared statements we'll hold on to.
	const STATEMENT_CACHE_DEFAULT_CAPACITY: usize = 16;
	
	/// A typedef of the result returned by many methods.
	pub type Result<T, E = Error> = result::Result<T, E>;

	/// See the [method documentation](#tymethod.optional).
	pub trait OptionalExtension<T> {
		/// Converts a `Result<T>` into a `Result<Option<T>>`.
		///
		/// By default, Rusqlite treats 0 rows being returned from a query that is
		/// expected to return 1 row as an error. This method will
		/// handle that error, and give you back an `Option<T>` instead.
		fn optional(self) -> Result<Option<T>>;
	}

	impl<T> OptionalExtension<T> for Result<T> {
		fn optional(self) -> Result<Option<T>> {
			match self {
				Ok(value) => Ok(Some(value)),
				Err(Error::QueryReturnedNoRows) => Ok(None),
				Err(e) => Err(e),
			}
		}
	}

	unsafe fn errmsg_to_string(errmsg: *const c_char) -> String {
		CStr::from_ptr(errmsg).to_string_lossy().into_owned()
	} 

	/// Returns `Ok((string ptr, len as c_int, SQLITE_STATIC | SQLITE_TRANSIENT))`
	/// normally.
	/// Returns error if the string is too large for sqlite.
	/// The `sqlite3_destructor_type` item is always `SQLITE_TRANSIENT` unless
	/// the string was empty (in which case it's `SQLITE_STATIC`, and the ptr is
	/// static).
	fn str_for_sqlite(s: &[u8]) -> Result<(*const c_char, c_int, ffi::sqlite3_destructor_type)> {
		let len = len_as_c_int(s.len())?;
		let (ptr, dtor_info) = if len != 0 {
			(s.as_ptr().cast::<c_char>(), ffi::SQLITE_TRANSIENT())
		} else {
			// Return a pointer guaranteed to live forever
			("".as_ptr().cast::<c_char>(), ffi::SQLITE_STATIC())
		};
		Ok((ptr, len, dtor_info))
	}

	// Helper to cast to c_int safely, returning the correct error type if the cast
	// failed.
	fn len_as_c_int(len: usize) -> Result<c_int> {
		if len >= (c_int::MAX as usize) {
			Err(err!(ffi::SQLITE_TOOBIG))
		} else {
			Ok(len as c_int)
		}
	}

	#[cfg(unix)]
	fn path_to_cstring(p: &Path) -> Result<CString> {
		use std::os::unix::ffi::OsStrExt;
		Ok(CString::new(p.as_os_str().as_bytes())?)
	}

	#[cfg(not(unix))]
	fn path_to_cstring(p: &Path) -> Result<CString> {
		let s = p.to_str().ok_or_else(|| Error::InvalidPath(p.to_owned()))?;
		Ok(CString::new(s)?)
	}

	/// Shorthand for `Main` database.
	pub const MAIN_DB: &CStr = c"main";
	/// Shorthand for `Temp` database.
	pub const TEMP_DB: &CStr = c"temp";

	/// A connection to a SQLite database.
	pub struct Connection {
		db: RefCell<InnerConnection>,
		cache: StatementCache,
		transaction_behavior: TransactionBehavior,
	}

	unsafe impl Send for Connection {}

	impl Drop for Connection {
		#[inline]
		fn drop(&mut self) {
			self.flush_prepared_statement_cache();
		}
	}

	impl Connection {
		/// Open a new connection to a SQLite database. If a database does not exist
		/// at the path, one is created.
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// fn open_my_db() -> Result<()> {
		///     let path = "./my_db.db3";
		///     let db = Connection::open(path)?;
		///     // Use the database somehow...
		///     println!("{}", db.is_autocommit());
		///     Ok(())
		/// }
		/// ```
		///
		/// # Flags
		///
		/// `Connection::open(path)` is equivalent to using
		/// [`Connection::open_with_flags`] with the default [`OpenFlags`]. That is,
		/// it's equivalent to:
		///
		/// ```ignore
		/// Connection::open_with_flags(
		///     path,
		///     OpenFlags::SQLITE_OPEN_READ_WRITE
		///         | OpenFlags::SQLITE_OPEN_CREATE
		///         | OpenFlags::SQLITE_OPEN_URI
		///         | OpenFlags::SQLITE_OPEN_NO_MUTEX,
		/// )
		/// ```
		///
		/// These flags have the following effects:
		///
		/// - Open the database for both reading or writing.
		/// - Create the database if one does not exist at the path.
		/// - Allow the filename to be interpreted as a URI (see <https://www.sqlite.org/uri.html#uri_filenames_in_sqlite>
		///   for details).
		/// - Disables the use of a per-connection mutex.
		///
		///   Rusqlite enforces thread-safety at compile time, so additional
		///   locking is not needed and provides no benefit. (See the
		///   documentation on [`OpenFlags::SQLITE_OPEN_FULL_MUTEX`] for some
		///   additional discussion about this).
		///
		/// Most of these are also the default settings for the C API, although
		/// technically the default locking behavior is controlled by the flags used
		/// when compiling SQLite -- rather than let it vary, we choose `NO_MUTEX`
		/// because it's a fairly clearly the best choice for users of this library.
		///
		/// # Failure
		///
		/// Will return `Err` if `path` cannot be converted to a C-compatible string
		/// or if the underlying SQLite open call fails.
		#[inline]
		pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
			let flags = OpenFlags::default();
			Self::open_with_flags(path, flags)
		}

		/// Open a new connection to an in-memory SQLite database.
		///
		/// # Failure
		///
		/// Will return `Err` if the underlying SQLite open call fails.
		#[inline]
		pub fn open_in_memory() -> Result<Self> {
			let flags = OpenFlags::default();
			Self::open_in_memory_with_flags(flags)
		}

		/// Open a new connection to a SQLite database.
		///
		/// [Database Connection](http://www.sqlite.org/c3ref/open.html) for a description of valid
		/// flag combinations.
		///
		/// # Failure
		///
		/// Will return `Err` if `path` cannot be converted to a C-compatible
		/// string or if the underlying SQLite open call fails.
		#[inline]
		pub fn open_with_flags<P: AsRef<Path>>(path: P, flags: OpenFlags) -> Result<Self> {
			let c_path = path_to_cstring(path.as_ref())?;
			InnerConnection::open_with_flags(&c_path, flags, None).map(|db| Self {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}

		/// Open a new connection to a SQLite database using the specific flags and
		/// vfs name.
		///
		/// [Database Connection](http://www.sqlite.org/c3ref/open.html) for a description of valid
		/// flag combinations.
		///
		/// # Failure
		///
		/// Will return `Err` if either `path` or `vfs` cannot be converted to a
		/// C-compatible string or if the underlying SQLite open call fails.
		#[inline]
		pub fn open_with_flags_and_vfs<P: AsRef<Path>, V: Name>(
			path: P,
			flags: OpenFlags,
			vfs: V,
		) -> Result<Self> {
			let c_path = path_to_cstring(path.as_ref())?;
			let c_vfs = vfs.as_cstr()?;
			InnerConnection::open_with_flags(&c_path, flags, Some(&c_vfs)).map(|db| Self {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}

		/// Open a new connection to an in-memory SQLite database.
		///
		/// [Database Connection](http://www.sqlite.org/c3ref/open.html) for a description of valid
		/// flag combinations.
		///
		/// # Failure
		///
		/// Will return `Err` if the underlying SQLite open call fails.
		#[inline]
		pub fn open_in_memory_with_flags(flags: OpenFlags) -> Result<Self> {
			Self::open_with_flags(":memory:", flags)
		}

		/// Open a new connection to an in-memory SQLite database using the specific
		/// flags and vfs name.
		///
		/// [Database Connection](http://www.sqlite.org/c3ref/open.html) for a description of valid
		/// flag combinations.
		///
		/// # Failure
		///
		/// Will return `Err` if `vfs` cannot be converted to a C-compatible
		/// string or if the underlying SQLite open call fails.
		#[inline]
		pub fn open_in_memory_with_flags_and_vfs<V: Name>(flags: OpenFlags, vfs: V) -> Result<Self> {
			Self::open_with_flags_and_vfs(":memory:", flags, vfs)
		}

		/// Convenience method to run multiple SQL statements (that cannot take any
		/// parameters).
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// fn create_tables(conn: &Connection) -> Result<()> {
		///     conn.execute_batch(
		///         "BEGIN;
		///          CREATE TABLE foo(x INTEGER);
		///          CREATE TABLE bar(y TEXT);
		///          COMMIT;",
		///     )
		/// }
		/// ```
		///
		/// # Failure
		///
		/// Will return `Err` if `sql` cannot be converted to a C-compatible string
		/// or if the underlying SQLite call fails.
		pub fn execute_batch(&self, sql: &str) -> Result<()> {
			let mut sql = sql;
			while !sql.is_empty() {
				let (stmt, tail) = self
					.db
					.borrow_mut()
					.prepare(self, sql, PrepFlags::default())?;
				if !stmt.stmt.is_null() && stmt.step()? {
					// Some PRAGMA may return rows
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
		///
		/// On success, returns the number of rows that were changed or inserted or
		/// deleted (via `sqlite3_changes`).
		///
		/// ## Example
		///
		/// ### With positional params
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection};
		/// fn update_rows(conn: &Connection) {
		///     match conn.execute("UPDATE foo SET bar = 'baz' WHERE qux = ?1", [1i32]) {
		///         Ok(updated) => println!("{} rows were updated", updated),
		///         Err(err) => println!("update failed: {}", err),
		///     }
		/// }
		/// ```
		///
		/// ### With positional params of varying types
		///
		/// ```rust,no_run
		/// # use rusqlite::{params, Connection};
		/// fn update_rows(conn: &Connection) {
		///     match conn.execute(
		///         "UPDATE foo SET bar = 'baz' WHERE qux = ?1 AND quux = ?2",
		///         params![1i32, 1.5f64],
		///     ) {
		///         Ok(updated) => println!("{} rows were updated", updated),
		///         Err(err) => println!("update failed: {}", err),
		///     }
		/// }
		/// ```
		///
		/// ### With named params
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// fn insert(conn: &Connection) -> Result<usize> {
		///     conn.execute(
		///         "INSERT INTO test (name) VALUES (:name)",
		///         &[(":name", "one")],
		///     )
		/// }
		/// ```
		///
		/// # Failure
		///
		/// Will return `Err` if `sql` cannot be converted to a C-compatible string
		/// or if the underlying SQLite call fails.
		#[inline]
		pub fn execute<P: Params>(&self, sql: &str, params: P) -> Result<usize> {
			self.prepare(sql).and_then(|mut stmt| stmt.execute(params))
		}

		/// Returns the path to the database file, if one exists and is known.
		///
		/// Returns `Some("")` for a temporary or in-memory database.
		///
		/// Note that in some cases [PRAGMA
		/// database_list](https://sqlite.org/pragma.html#pragma_database_list) is
		/// likely to be more robust.
		#[inline]
		pub fn path(&self) -> Option<&str> {
			unsafe {
				crate::inner_connection::db_filename(std::marker::PhantomData, self.handle(), MAIN_DB)
			}
		}

		/// Attempts to free as much heap memory as possible from the database
		/// connection.
		///
		/// This calls [`sqlite3_db_release_memory`](https://www.sqlite.org/c3ref/db_release_memory.html).
		#[inline]
		pub fn release_memory(&self) -> Result<()> {
			self.db.borrow_mut().release_memory()
		}

		/// Get the SQLite rowid of the most recent successful INSERT.
		///
		/// Uses [sqlite3_last_insert_rowid](https://www.sqlite.org/c3ref/last_insert_rowid.html) under
		/// the hood.
		#[inline]
		pub fn last_insert_rowid(&self) -> i64 {
			self.db.borrow_mut().last_insert_rowid()
		}

		/// Convenience method to execute a query that is expected to return a
		/// single row.
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Result, Connection};
		/// fn preferred_locale(conn: &Connection) -> Result<String> {
		///     conn.query_row(
		///         "SELECT value FROM preferences WHERE name='locale'",
		///         [],
		///         |row| row.get(0),
		///     )
		/// }
		/// ```
		///
		/// If the query returns more than one row, all rows except the first are
		/// ignored.
		///
		/// Returns `Err(QueryReturnedNoRows)` if no results are returned. If the
		/// query truly is optional, you can call `.optional()` on the result of
		/// this to get a `Result<Option<T>>`.
		///
		/// # Failure
		///
		/// Will return `Err` if `sql` cannot be converted to a C-compatible string
		/// or if the underlying SQLite call fails.
		#[inline]
		pub fn query_row<T, P, F>(&self, sql: &str, params: P, f: F) -> Result<T>
		where
			P: Params,
			F: FnOnce(&Row<'_>) -> Result<T>,
		{
			let mut stmt = self.prepare(sql)?;
			stmt.query_row(params, f)
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
		pub fn query_one<T, P, F>(&self, sql: &str, params: P, f: F) -> Result<T>
		where
			P: Params,
			F: FnOnce(&Row<'_>) -> Result<T>,
		{
			let mut stmt = self.prepare(sql)?;
			stmt.query_one(params, f)
		}

		// https://sqlite.org/tclsqlite.html#onecolumn
		#[cfg(test)]
		pub(crate) fn one_column<T, P>(&self, sql: &str, params: P) -> Result<T>
		where
			T: types::FromSql,
			P: Params,
		{
			self.query_one(sql, params, |r| r.get(0))
		}

		/// Convenience method to execute a query that is expected to return a
		/// single row, and execute a mapping via `f` on that returned row with
		/// the possibility of failure. The `Result` type of `f` must implement
		/// `std::convert::From<Error>`.
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Result, Connection};
		/// fn preferred_locale(conn: &Connection) -> Result<String> {
		///     conn.query_row_and_then(
		///         "SELECT value FROM preferences WHERE name='locale'",
		///         [],
		///         |row| row.get(0),
		///     )
		/// }
		/// ```
		///
		/// If the query returns more than one row, all rows except the first are
		/// ignored.
		///
		/// # Failure
		///
		/// Will return `Err` if `sql` cannot be converted to a C-compatible string
		/// or if the underlying SQLite call fails.
		#[inline]
		pub fn query_row_and_then<T, E, P, F>(&self, sql: &str, params: P, f: F) -> Result<T, E>
		where
			P: Params,
			F: FnOnce(&Row<'_>) -> Result<T, E>,
			E: From<Error>,
		{
			let mut stmt = self.prepare(sql)?;
			let mut rows = stmt.query(params)?;

			rows.get_expected_row().map_err(E::from).and_then(f)
		}

		/// Prepare a SQL statement for execution.
		///
		/// ## Example
		///
		/// ```rust,no_run
		/// # use rusqlite::{Connection, Result};
		/// fn insert_new_people(conn: &Connection) -> Result<()> {
		///     let mut stmt = conn.prepare("INSERT INTO People (name) VALUES (?1)")?;
		///     stmt.execute(["Joe Smith"])?;
		///     stmt.execute(["Bob Jones"])?;
		///     Ok(())
		/// }
		/// ```
		///
		/// # Failure
		///
		/// Will return `Err` if `sql` cannot be converted to a C-compatible string
		/// or if the underlying SQLite call fails.
		#[inline]
		pub fn prepare(&self, sql: &str) -> Result<Statement<'_>> {
			self.prepare_with_flags(sql, PrepFlags::default())
		}

		/// Prepare a SQL statement for execution.
		///
		/// # Failure
		///
		/// Will return `Err` if `sql` cannot be converted to a C-compatible string
		/// or if the underlying SQLite call fails.
		#[inline]
		pub fn prepare_with_flags(&self, sql: &str, flags: PrepFlags) -> Result<Statement<'_>> {
			let (stmt, tail) = self.db.borrow_mut().prepare(self, sql, flags)?;
			if tail != 0 && !self.prepare(&sql[tail..])?.stmt.is_null() {
				Err(Error::MultipleStatement)
			} else {
				Ok(stmt)
			}
		}

		/// Close the SQLite connection.
		///
		/// This is functionally equivalent to the `Drop` implementation for
		/// `Connection` except that on failure, it returns an error and the
		/// connection itself (presumably so closing can be attempted again).
		///
		/// # Failure
		///
		/// Will return `Err` if the underlying SQLite call fails.
		#[allow(clippy::result_large_err)]
		#[inline]
		pub fn close(self) -> Result<(), (Self, Error)> {
			self.flush_prepared_statement_cache();
			let r = self.db.borrow_mut().close();
			r.map_err(move |err| (self, err))
		}

		/// Get access to the underlying SQLite database connection handle.
		///
		/// # Warning
		///
		/// You should not need to use this function. If you do need to, please
		/// [open an issue on the rusqlite repository](https://github.com/rusqlite/rusqlite/issues) and describe
		/// your use case.
		///
		/// # Safety
		///
		/// This function is unsafe because it gives you raw access
		/// to the SQLite connection, and what you do with it could impact the
		/// safety of this `Connection`.
		#[inline]
		pub unsafe fn handle(&self) -> *mut ffi::sqlite3 {
			self.db.borrow().db()
		}

		/// Create a `Connection` from a raw handle.
		///
		/// The underlying SQLite database connection handle will not be closed when
		/// the returned connection is dropped/closed.
		///
		/// # Safety
		///
		/// This function is unsafe because improper use may impact the Connection.
		#[inline]
		pub unsafe fn from_handle(db: *mut ffi::sqlite3) -> Result<Self> {
			let db = InnerConnection::new(db, false);
			Ok(Self {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}

		/// Create a `Connection` from a raw owned handle.
		///
		/// The returned connection will attempt to close the inner connection
		/// when dropped/closed. This function should only be called on connections
		/// owned by the caller.
		///
		/// # Safety
		///
		/// This function is unsafe because improper use may impact the Connection.
		/// In particular, it should only be called on connections created
		/// and owned by the caller, e.g. as a result of calling
		/// `ffi::sqlite3_open`().
		#[inline]
		pub unsafe fn from_handle_owned(db: *mut ffi::sqlite3) -> Result<Self> {
			let db = InnerConnection::new(db, true);
			Ok(Self {
				db: RefCell::new(db),
				cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
				transaction_behavior: TransactionBehavior::Deferred,
			})
		}

		/// Get access to a handle that can be used to interrupt long-running
		/// queries from another thread.
		#[inline]
		pub fn get_interrupt_handle(&self) -> InterruptHandle {
			self.db.borrow().get_interrupt_handle()
		}

		#[inline]
		fn decode_result(&self, code: c_int) -> Result<()> {
			self.db.borrow().decode_result(code)
		}

		/// Return the number of rows modified, inserted or deleted by the most
		/// recently completed INSERT, UPDATE or DELETE statement on the database
		/// connection.
		///
		/// See <https://www.sqlite.org/c3ref/changes.html>
		#[inline]
		pub fn changes(&self) -> u64 {
			self.db.borrow().changes()
		}

		/// Return the total number of rows modified, inserted or deleted by all
		/// completed INSERT, UPDATE or DELETE statements since the database
		/// connection was opened, including those executed as part of trigger programs.
		///
		/// See <https://www.sqlite.org/c3ref/total_changes.html>
		#[inline]
		pub fn total_changes(&self) -> u64 {
			self.db.borrow().total_changes()
		}

		/// Test for auto-commit mode.
		/// Autocommit mode is on by default.
		#[inline]
		pub fn is_autocommit(&self) -> bool {
			self.db.borrow().is_autocommit()
		}

		/// Determine if all associated prepared statements have been reset.
		#[inline]
		pub fn is_busy(&self) -> bool {
			self.db.borrow().is_busy()
		}

		/// Flush caches to disk mid-transaction
		pub fn cache_flush(&self) -> Result<()> {
			self.db.borrow_mut().cache_flush()
		}

		/// Determine if a database is read-only
		pub fn is_readonly<N: Name>(&self, db_name: N) -> Result<bool> {
			self.db.borrow().db_readonly(db_name)
		}

		/// Return the schema name for a database connection
		///
		/// ## Failure
		///
		/// Return an `Error::InvalidDatabaseIndex` if `index` is out of range.
		pub fn db_name(&self, index: usize) -> Result<String> {
			unsafe {
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
		pub fn is_interrupted(&self) -> bool {
			self.db.borrow().is_interrupted()
		}
	}

	impl fmt::Debug for Connection {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.debug_struct("Connection")
				.field("path", &self.path())
				.finish()
		}
	}

	/// Batch fallible iterator
	///
	/// # Warning
	///
	/// There is no recovery on parsing error, when a invalid statement is found in `sql`, SQLite cannot jump to the next statement.
	/// So you should break the loop when an error is raised by the `next` method.
	///
	/// ```rust
	/// use fallible_iterator::FallibleIterator;
	/// use rusqlite::{Batch, Connection, Result};
	///
	/// fn main() -> Result<()> {
	///     let conn = Connection::open_in_memory()?;
	///     let sql = r"
	///     CREATE TABLE tbl1 (col);
	///     CREATE TABLE tbl2 (col);
	///     ";
	///     let mut batch = Batch::new(&conn, sql);
	///     while let Some(mut stmt) = batch.next()? {
	///         stmt.execute([])?;
	///     }
	///     Ok(())
	/// }
	/// ```
	#[derive(Debug)]
	pub struct Batch<'conn, 'sql> {
		conn: &'conn Connection,
		sql: &'sql str,
		tail: usize,
	}

	impl<'conn, 'sql> Batch<'conn, 'sql> {
		/// Constructor
		pub fn new(conn: &'conn Connection, sql: &'sql str) -> Self {
			Batch { conn, sql, tail: 0 }
		}
	}
	impl<'conn> fallible_iterator::FallibleIterator for Batch<'conn, '_> {
		type Error = Error;
		type Item = Statement<'conn>;

		/// Iterates on each batch statements.
		///
		/// Returns `Ok(None)` when batch is completed.
		fn next(&mut self) -> Result<Option<Statement<'conn>>> {
			while self.tail < self.sql.len() {
				let sql = &self.sql[self.tail..];
				let (next, tail) =
					self.conn
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

	bitflags::bitflags! {
		/// Flags for opening SQLite database connections. See
		/// [sqlite3_open_v2](https://www.sqlite.org/c3ref/open.html) for details.
		///
		/// The default open flags are `SQLITE_OPEN_READ_WRITE | SQLITE_OPEN_CREATE
		/// | SQLITE_OPEN_URI | SQLITE_OPEN_NO_MUTEX`. See [`Connection::open`] for
		/// some discussion about these flags.
		#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
		#[repr(C)]
		pub struct OpenFlags: c_int {
			/// The database is opened in read-only mode.
			/// If the database does not already exist, an error is returned.
			const SQLITE_OPEN_READ_ONLY = ffi::SQLITE_OPEN_READONLY;
			/// The database is opened for reading and writing if possible,
			/// or reading only if the file is write-protected by the operating system.
			/// In either case the database must already exist, otherwise an error is returned.
			const SQLITE_OPEN_READ_WRITE = ffi::SQLITE_OPEN_READWRITE;
			/// The database is created if it does not already exist
			const SQLITE_OPEN_CREATE = ffi::SQLITE_OPEN_CREATE;
			/// The filename can be interpreted as a URI if this flag is set.
			const SQLITE_OPEN_URI = ffi::SQLITE_OPEN_URI;
			/// The database will be opened as an in-memory database.
			const SQLITE_OPEN_MEMORY = ffi::SQLITE_OPEN_MEMORY;
			/// The new database connection will not use a per-connection mutex (the
			/// connection will use the "multi-thread" threading mode, in SQLite
			/// parlance).
			///
			/// This is used by default, as proper `Send`/`Sync` usage (in
			/// particular, the fact that [`Connection`] does not implement `Sync`)
			/// ensures thread-safety without the need to perform locking around all
			/// calls.
			const SQLITE_OPEN_NO_MUTEX = ffi::SQLITE_OPEN_NOMUTEX;
			/// The new database connection will use a per-connection mutex -- the
			/// "serialized" threading mode, in SQLite parlance.
			///
			/// # Caveats
			///
			/// This flag should probably never be used with `rusqlite`, as we
			/// ensure thread-safety statically (we implement [`Send`] and not
			/// [`Sync`]).
			///
			/// Critically, even if this flag is used, the [`Connection`] is not
			/// safe to use across multiple threads simultaneously. To access a
			/// database from multiple threads, you should either create multiple
			/// connections, one for each thread (if you have very many threads,
			/// wrapping the `rusqlite::Connection` in a mutex is also reasonable).
			///
			/// This is both because of the additional per-connection state stored
			/// by `rusqlite` (for example, the prepared statement cache), and
			/// because not all of SQLites functions are fully thread safe, even in
			/// serialized/`SQLITE_OPEN_FULLMUTEX` mode.
			///
			/// All that said, it's fairly harmless to enable this flag with
			/// `rusqlite`, it will just slow things down while providing no
			/// benefit.
			const SQLITE_OPEN_FULL_MUTEX = ffi::SQLITE_OPEN_FULLMUTEX;
			/// The database is opened with shared cache enabled.
			///
			/// This is frequently useful for in-memory connections, but note that
			/// broadly speaking it's discouraged by SQLite itself, which states
			/// "Any use of shared cache is discouraged" in the official
			/// [documentation](https://www.sqlite.org/c3ref/enable_shared_cache.html).
			const SQLITE_OPEN_SHARED_CACHE = 0x0002_0000;
			/// The database is opened shared cache disabled.
			const SQLITE_OPEN_PRIVATE_CACHE = 0x0004_0000;
			/// The database filename is not allowed to be a symbolic link. (3.31.0)
			const SQLITE_OPEN_NOFOLLOW = 0x0100_0000;
			/// Extended result codes. (3.37.0)
			const SQLITE_OPEN_EXRESCODE = 0x0200_0000;
		}
	}

	impl Default for OpenFlags {
		#[inline]
		fn default() -> Self {
			// Note: update the `Connection::open` and top-level `OpenFlags` docs if
			// you change these.
			Self::SQLITE_OPEN_READ_WRITE
				| Self::SQLITE_OPEN_CREATE
				| Self::SQLITE_OPEN_NO_MUTEX
				| Self::SQLITE_OPEN_URI
		}
	}

	bitflags::bitflags! {
		/// Prepare flags. See
		/// [sqlite3_prepare_v3](https://sqlite.org/c3ref/c_prepare_normalize.html) for details.
		#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
		#[repr(C)]
		pub struct PrepFlags: c_uint {
			/// A hint to the query planner that the prepared statement will be retained for a long time and probably reused many times.
			const SQLITE_PREPARE_PERSISTENT = 0x01;
			/// Causes the SQL compiler to return an error (error code SQLITE_ERROR) if the statement uses any virtual tables.
			const SQLITE_PREPARE_NO_VTAB = 0x04;
			/// Prevents SQL compiler errors from being sent to the error log.
			const SQLITE_PREPARE_DONT_LOG = 0x10;
		}
	}

	/// Allows interrupting a long-running computation.
	pub struct InterruptHandle {
		db_lock: Arc<Mutex<*mut ffi::sqlite3>>,
	}

	unsafe impl Send for InterruptHandle {}
	unsafe impl Sync for InterruptHandle {}

	impl InterruptHandle {
		/// Interrupt the query currently executing on another thread.
		pub fn interrupt(&self) {
			let db_handle = self.db_lock.lock().unwrap();
			if !db_handle.is_null() {
				unsafe { ffi::sqlite3_interrupt(*db_handle) }
			}
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


