module Prelude where

infixr 5 ++

class  (MonadZero m) => MonadPlus m where
   (++)         :: m a -> m a -> m a
