module Prelude(Monad(..)) where

infixr 1 >>, >>=

class Monad m where
    (>>=)	:: m a -> (a -> m b) -> m b
    (>>)	:: m a -> m b -> m b
    return      :: a -> m a

    m >> k 	= m >>= \ _ -> k

