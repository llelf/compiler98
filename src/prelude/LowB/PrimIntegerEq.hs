module Prelude where

-- primIntegerEq  primitive 2 :: Integer -> Integer -> Bool

foreign import "primIntegerEqC" primIntegerEq  :: Integer -> Integer -> Bool


