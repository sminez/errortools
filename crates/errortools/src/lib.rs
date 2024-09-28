//! Extensions for Error values

#[doc(hidden)]
pub use errortools_proc::error_source_internal;
use std::error::Error;

pub trait ErrorTools: Error + Sized + 'static {
    /// Returns an iterator that yields the current error followed by the result
    /// of repeatedly calling [`Error::source`] until no further wrapped errors
    /// are returned.
    fn chain(&self) -> Chain<'_> {
        Chain {
            current: Some(self),
        }
    }

    /// The lowest level source of this error.
    ///
    /// This is the last element of iterator returned by `chain()`.
    fn root_cause(&self) -> &(dyn Error + 'static) {
        self.chain().last().unwrap()
    }

    /// Search through this error and each of its sources until a target error
    /// type is located. If the target type is not in this error chain, this
    /// method will return None.
    fn contained<E: Error + 'static>(&self) -> Option<&E> {
        for error in self.chain() {
            let downcast = error.downcast_ref::<E>();
            if downcast.is_some() {
                return downcast;
            }
        }

        None
    }

    /// Convert this error and all nested source errors to their
    /// [`std::fmt:Display`] string representation as a newline delimited
    /// string.
    fn display_all(&self) -> String {
        let v: Vec<String> = self.chain().map(|e| e.to_string()).collect();

        v.join("\n")
    }
}

// Blanket impl for all std::error::Error impls
impl<T: 'static> ErrorTools for T where T: Error {}

/// An iterator that yields the current error followed by the result of
/// repeatedly calling [`Error::source`] until no further wrapped errors are
/// returned.
///
/// ## NOTE
/// Using [`thiserror::Error`] and the `error(transparent)` attribute will set
/// the source of any annotated error variants to be deferred through to the
/// wrapped error contained within. This causes [`Chain`] to skip those levels
/// of the error chain while iterating, requiring you to explicitly match against
/// a nested error and its parent.
///
/// To only defer the [`std::fmt::Display`] implementation to the wrapped error,
/// you can use `#[error("0")]` instead.
#[derive(Debug, Clone)]
pub struct Chain<'a> {
    current: Option<&'a (dyn Error + 'static)>,
}

impl<'a> Iterator for Chain<'a> {
    type Item = &'a (dyn Error + 'static);

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        self.current = self.current.and_then(Error::source);

        current
    }
}

/// Check the chain of [`Error::source`] values for a given error in priority
/// order. The first matching pattern will be evalutated (similar to a match
/// statement) but note that `match` is _not_ used under the hood to make this
/// work.
///
/// Values may be returned from the arms of this macro so long as types are
/// consistent. If no `@no_match` arm is provided, this macro will panic if none
/// of the provided patterns match an Error within the given error chain.
#[macro_export]
macro_rules! error_source {
    // Default behaviour if no @no_match arm is given is to panic
    (if $err:ident contains {$($p:pat => $target:expr,)+}) => {
        $crate::error_source_internal! {
            if $err contains { $($p => $target,)+ }
            default => panic!("no matching error variant found for {:?}", $err),
        }
    };

    ($($tokens:tt)*) => {$crate::error_source_internal!($($tokens)*)};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, thiserror::Error)]
    pub enum TopLevel {
        #[error("Top level tuple with message: {0}")]
        Top(&'static str),

        #[error("Top level wrapped Mid")]
        Mid(#[from] MidLevel),

        #[error("{0}")]
        DeferredDisplay(#[from] BaseLevel),

        #[error(transparent)]
        Transparent(#[from] Transparent),
    }

    #[derive(Debug, thiserror::Error)]
    pub enum MidLevel {
        #[error("Mid level struct with message: {msg}")]
        Mid { msg: &'static str },

        #[error("Mid level wrapped Base")]
        Base(#[from] BaseLevel),
    }

    #[derive(Debug, thiserror::Error)]
    pub enum BaseLevel {
        #[error("Base level struct with message: {msg}")]
        Struct { msg: &'static str },

        #[error("Base level tuple with message: {0}")]
        Tuple(&'static str),
    }

    #[derive(Debug, thiserror::Error)]
    pub enum Transparent {
        #[error("Transparent tuple with message: {0}")]
        Tuple(&'static str),
    }

    #[test]
    fn chain_works() {
        let e = TopLevel::Mid(MidLevel::Base(BaseLevel::Struct { msg: "test" }));
        let messages: Vec<String> = e.chain().map(|cause| cause.to_string()).collect();

        assert_eq!(
            messages,
            vec![
                "Top level wrapped Mid",
                "Mid level wrapped Base",
                "Base level struct with message: test"
            ]
        )
    }

    #[test]
    fn chain_works_when_only_deferring_the_display_impl() {
        let e = TopLevel::DeferredDisplay(BaseLevel::Struct { msg: "test" });
        let messages: Vec<String> = e.chain().map(|cause| cause.to_string()).collect();

        assert_eq!(
            messages,
            vec![
                "Base level struct with message: test",
                "Base level struct with message: test"
            ]
        )
    }

    #[test]
    fn root_cause_works() {
        let e = TopLevel::Mid(MidLevel::Base(BaseLevel::Tuple("test")));

        assert_eq!(
            e.root_cause().to_string(),
            "Base level tuple with message: test"
        );
    }

    #[test]
    fn contained_direct_works() {
        let e = TopLevel::Top("test");

        if let Some(&TopLevel::Top(msg)) = e.contained() {
            assert_eq!(msg, "test");
        } else {
            panic!("should have matched top level error");
        }
    }

    #[test]
    fn contained_struct_works() {
        let e = TopLevel::Mid(MidLevel::Mid { msg: "test" });

        if let Some(&MidLevel::Mid { msg }) = e.contained() {
            assert_eq!(msg, "test");
        } else {
            panic!("should have matched wrapped struct error");
        }
    }

    #[test]
    fn contained_tuple_works() {
        let e = TopLevel::Mid(MidLevel::Base(BaseLevel::Tuple("test")));

        if let Some(&BaseLevel::Tuple(msg)) = e.contained() {
            assert_eq!(msg, "test");
        } else {
            panic!("should have matched wrapped struct error");
        }
    }

    #[test]
    fn error_source_macro_works() {
        let e = TopLevel::Mid(MidLevel::Base(BaseLevel::Tuple("test")));

        error_source! {
            if e contains {
                BaseLevel::Tuple(msg) => assert_eq!(*msg, "test"),
                MidLevel::Base(_) => panic!("should have matched the base error instead"),
            }
        }

        error_source! {
            if e contains {
                MidLevel::Base(_) => (),
                BaseLevel::Tuple(_) => panic!("should have matched the mid-level error first"),
            }
        }
    }

    #[test]
    fn error_source_macro_works_when_returning_a_value() {
        let e = TopLevel::Mid(MidLevel::Base(BaseLevel::Tuple("test")));

        // base first
        let level = error_source! {
            if e contains {
                BaseLevel::Tuple(_) => "base",
                MidLevel::Base(_) => "mid",
            }
        };

        assert_eq!(level, "base");

        // mid first
        let level = error_source! {
            if e contains {
                MidLevel::Base(_) => "mid",
                BaseLevel::Tuple(_) => "base",
            }
        };

        assert_eq!(level, "mid");

        // no match
        let level = error_source! {
            if e contains {
                MidLevel::Mid { .. } => "mid",
                BaseLevel::Struct { .. } => "base",
            }

            default => "default",
        };

        assert_eq!(level, "default");
    }

    // NOTE: the following two tests are more about documenting behaviour we
    // need to be aware of than ensuring that the behaviour itself is
    // maintained. Ideally we want to be able to use #[error(transparent)] (or
    // at least, not have the use of it break our error handling in this way)
    // but given they way that it intereacts with `Error::source` it is
    // something we need to be careful about.

    #[test]
    fn contained_transparent_requires_explicit_nesting_to_match() {
        let e = TopLevel::Transparent(Transparent::Tuple("test"));

        if let Some(&TopLevel::Transparent(Transparent::Tuple(msg))) = e.contained() {
            assert_eq!(msg, "test");
        } else {
            panic!("should have matched explicitly nested transparent error");
        }
    }

    #[test]
    fn contained_transparent_without_explicit_nesting_does_not_match() {
        let e = TopLevel::Transparent(Transparent::Tuple("test"));

        if let Some(&Transparent::Tuple(_)) = e.contained() {
            panic!("we are now able to match nested transparent errors implicitly");
        }
    }

    #[test]
    fn display_all_works() {
        let e = TopLevel::Mid(MidLevel::Base(BaseLevel::Struct { msg: "test" }));

        assert_eq!(
            e.display_all(),
            "Top level wrapped Mid\nMid level wrapped Base\nBase level struct with message: test"
        )
    }
}
