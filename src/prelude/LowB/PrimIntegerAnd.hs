module Prelude where

-- primIntegerAnd primitive 2 :: Integer -> Integer -> Integer

foreign import "primIntegerAndC" primIntegerAnd :: Integer -> Integer -> Integer

