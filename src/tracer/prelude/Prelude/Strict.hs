module Prelude(($!)) where

infixr 0 $!

--($!)      :: (a -> b) -> a -> b
f $! x  =  x `seq` f x

