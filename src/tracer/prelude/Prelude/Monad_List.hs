module Prelude(Monad(..)) where

instance Monad [] where
  m >>= k = concat (map k m)
  return x = x:[]
