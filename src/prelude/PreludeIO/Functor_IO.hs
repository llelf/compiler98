module Prelude(Functor(..)) where

-- import CFunctor

instance  Functor IO where
   fmap f x              = x >>= (return . f)
