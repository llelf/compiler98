module Prelude where

-- primIntegerAbs primitive 1 :: Integer -> Integer

foreign import "primIntegerAbsC" primIntegerAbs :: Integer -> Integer
