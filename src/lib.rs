pub mod ass;

pub mod either {

    pub use Either::*;
    pub enum Either<T, U> {
        L(T),
        R(U),
    }

    impl<T, U> From<Either<T, U>> for Result<T, U> {
        fn from(e: Either<T, U>) -> Result<T, U> {
            match e {
                L(t) => Ok(t),
                R(u) => Err(u),
            }
        }
    }

    impl<T, U> Either<T, U> {
        pub fn left(self) -> Option<T> {
            if let L(t) = self {
                Some(t)
            } else {
                None
            }
        }

        pub fn right(self) -> Option<U> {
            if let R(t) = self {
                Some(t)
            } else {
                None
            }
        }

        pub fn swap(self) -> Either<U, T> {
            match self {
                L(t) => R(t),
                R(u) => L(u),
            }
        }

        pub fn as_ref(&self) -> Either<&T, &U> {
            match self {
                L(t) => L(t),
                R(u) => R(u),
            }
        }

        pub fn as_mut(&mut self) -> Either<&mut T, &mut U> {
            match self {
                L(l) => L(l),
                R(u) => R(u),
            }
        }

        pub fn map_left<M: FnOnce(T) -> Either<T, U>>(self, m: M) -> Either<T, U> {
            match self {
                L(l) => m(l),
                a => a,
            }
        }

        pub fn map_right<M: FnOnce(U) -> Either<T, U>>(self, m: M) -> Either<T, U> {
            match self {
                R(r) => m(r),
                a => a,
            }
        }
    }
}
