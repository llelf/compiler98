module Prelude(Functor(..)) where

instance Functor [] where
    map f []             =  []
    map f (x:xs)         =  f x : map f xs
