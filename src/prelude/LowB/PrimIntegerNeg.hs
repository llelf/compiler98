module Prelude where

-- primIntegerNeg primitive 1 :: Integer -> Integer

foreign import "primIntegerNegC" primIntegerNeg :: Integer -> Integer

