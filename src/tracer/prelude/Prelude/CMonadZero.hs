module Prelude where

class (Monad m) => MonadZero m where
    zero 	:: m a
