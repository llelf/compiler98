module Prelude where

-- primIntegerFromInt primitive 1 :: Int -> Integer

foreign import "primIntegerFromIntC" primIntegerFromInt :: Int -> Integer
