module Prelude(MonadZero(..)) where

instance MonadZero [] where
  zero = []
