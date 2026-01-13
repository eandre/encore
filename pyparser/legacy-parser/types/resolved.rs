//! Lazy resolution tracking.
//!
//! This module provides the `Resolved` enum which tracks whether type resolution
//! produced a new value, modified an existing value, or returned the original unchanged.
//! This optimization avoids unnecessary allocations during type resolution.

use std::borrow::Cow;

/// Result of a type resolution operation.
///
/// This enum tracks whether the resolution produced a new value, modified
/// an existing borrowed value, or returned the original unchanged.
///
/// # Usage
///
/// ```ignore
/// let result = ctx.concrete(&typ);
/// match result {
///     Resolved::New(owned) => /* Use newly allocated type */,
///     Resolved::Changed(borrowed) => /* Type was modified but borrowed */,
///     Resolved::Same(borrowed) => /* Type unchanged, use original reference */,
/// }
/// ```
pub enum Resolved<'a, B: ?Sized + ToOwned> {
    /// A newly created owned value.
    New(<B as ToOwned>::Owned),
    /// A borrowed value that was modified from the original.
    Changed(&'a B),
    /// The original value, unchanged.
    Same(&'a B),
}

impl<'a, B: ?Sized + ToOwned> Resolved<'a, B> {
    /// Returns true if this is a new value.
    pub fn is_new(&self) -> bool {
        matches!(self, Resolved::New(_))
    }

    /// Returns true if the value was changed (either New or Changed).
    pub fn is_changed(&self) -> bool {
        !matches!(self, Resolved::Same(_))
    }

    /// Returns true if the value is the same as the original.
    pub fn is_same(&self) -> bool {
        matches!(self, Resolved::Same(_))
    }

    /// Get a reference to the value, regardless of variant.
    pub fn as_ref(&self) -> &B
    where
        <B as ToOwned>::Owned: AsRef<B>,
    {
        match self {
            Resolved::New(owned) => owned.as_ref(),
            Resolved::Changed(borrowed) | Resolved::Same(borrowed) => borrowed,
        }
    }

    /// Map the resolved value with a function.
    pub fn map<F, U>(self, f: F) -> Resolved<'a, U>
    where
        U: ToOwned,
        F: FnOnce(<B as ToOwned>::Owned) -> <U as ToOwned>::Owned,
        <B as ToOwned>::Owned: Clone,
    {
        match self {
            Resolved::New(owned) => Resolved::New(f(owned)),
            Resolved::Changed(borrowed) => Resolved::New(f(borrowed.to_owned())),
            Resolved::Same(borrowed) => Resolved::New(f(borrowed.to_owned())),
        }
    }

    /// Convert to an owned value, cloning if necessary.
    pub fn into_owned(self) -> <B as ToOwned>::Owned {
        match self {
            Resolved::New(owned) => owned,
            Resolved::Changed(borrowed) | Resolved::Same(borrowed) => borrowed.to_owned(),
        }
    }
}

impl<'a, B: ?Sized + ToOwned> From<Resolved<'a, B>> for Cow<'a, B> {
    fn from(val: Resolved<'a, B>) -> Self {
        match val {
            Resolved::New(owned) => Cow::Owned(owned),
            Resolved::Changed(b) | Resolved::Same(b) => Cow::Borrowed(b),
        }
    }
}

impl<'a, B: Clone> Resolved<'a, B> {
    /// Convert a Resolved<B> to Resolved<T> by cloning and applying a conversion.
    pub fn map_clone<T, F>(self, f: F) -> T
    where
        F: FnOnce(B) -> T,
    {
        match self {
            Resolved::New(owned) => f(owned),
            Resolved::Changed(borrowed) | Resolved::Same(borrowed) => f(borrowed.clone()),
        }
    }
}

impl<'a, B: ?Sized + ToOwned + std::fmt::Debug> std::fmt::Debug for Resolved<'a, B>
where
    <B as ToOwned>::Owned: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Resolved::New(owned) => f.debug_tuple("New").field(owned).finish(),
            Resolved::Changed(borrowed) => f.debug_tuple("Changed").field(borrowed).finish(),
            Resolved::Same(borrowed) => f.debug_tuple("Same").field(borrowed).finish(),
        }
    }
}

/// Helper for combining multiple Resolved values.
pub struct ResolvedBuilder<'a, B: ?Sized + ToOwned> {
    any_changed: bool,
    _phantom: std::marker::PhantomData<&'a B>,
}

impl<'a, B: ?Sized + ToOwned> ResolvedBuilder<'a, B> {
    /// Create a new builder.
    pub fn new() -> Self {
        ResolvedBuilder {
            any_changed: false,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Track a resolved value, noting if it was changed.
    pub fn track(&mut self, resolved: &Resolved<'a, B>) {
        if resolved.is_changed() {
            self.any_changed = true;
        }
    }

    /// Returns true if any tracked value was changed.
    pub fn any_changed(&self) -> bool {
        self.any_changed
    }

    /// Finalize with an owned value, returning Changed if any were changed, else Same.
    pub fn finalize<'b>(self, original: &'b B, new_value: <B as ToOwned>::Owned) -> Resolved<'b, B> {
        if self.any_changed {
            Resolved::New(new_value)
        } else {
            Resolved::Same(original)
        }
    }
}

impl<'a, B: ?Sized + ToOwned> Default for ResolvedBuilder<'a, B> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolved_is_methods() {
        let new: Resolved<'_, str> = Resolved::New(String::from("new"));
        assert!(new.is_new());
        assert!(new.is_changed());
        assert!(!new.is_same());

        let same: Resolved<'_, str> = Resolved::Same("same");
        assert!(!same.is_new());
        assert!(!same.is_changed());
        assert!(same.is_same());

        let changed: Resolved<'_, str> = Resolved::Changed("changed");
        assert!(!changed.is_new());
        assert!(changed.is_changed());
        assert!(!changed.is_same());
    }

    #[test]
    fn test_resolved_into_owned() {
        let new: Resolved<'_, str> = Resolved::New(String::from("new"));
        assert_eq!(new.into_owned(), "new");

        let same: Resolved<'_, str> = Resolved::Same("same");
        assert_eq!(same.into_owned(), "same");
    }

    #[test]
    fn test_resolved_to_cow() {
        let new: Resolved<'_, str> = Resolved::New(String::from("new"));
        let cow: Cow<'_, str> = new.into();
        assert!(matches!(cow, Cow::Owned(_)));

        let same: Resolved<'_, str> = Resolved::Same("same");
        let cow: Cow<'_, str> = same.into();
        assert!(matches!(cow, Cow::Borrowed(_)));
    }

    #[test]
    fn test_resolved_builder() {
        let original = "original";
        let mut builder = ResolvedBuilder::<str>::new();

        let same: Resolved<'_, str> = Resolved::Same("a");
        builder.track(&same);
        assert!(!builder.any_changed());

        let changed: Resolved<'_, str> = Resolved::Changed("b");
        builder.track(&changed);
        assert!(builder.any_changed());

        let result = builder.finalize(original, String::from("result"));
        assert!(result.is_new());
    }
}
