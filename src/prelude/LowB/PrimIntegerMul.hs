module Prelude where

-- primIntegerMul primitive 2 :: Integer -> Integer -> Integer

foreign import "primIntegerMulC" primIntegerMul :: Integer -> Integer -> Integer

