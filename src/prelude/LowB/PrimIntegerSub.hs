module Prelude where

-- primIntegerSub primitive 2 :: Integer -> Integer -> Integer

foreign import "primIntegerSubC" primIntegerSub :: Integer -> Integer -> Integer
