module Prelude where

-- primIntegerAdd primitive 2 :: Integer -> Integer -> Integer

foreign import "primIntegerAddC" primIntegerAdd :: Integer -> Integer -> Integer
