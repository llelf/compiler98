module Prelude where

{-
filter          :: MonadZero m => (a -> Bool) -> m a -> m a
filter p        =  applyM (\x -> if p x then return x else zero)
-}

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs
