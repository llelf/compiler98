module Prelude where

-- primIntegerLe primitive 2 :: Integer -> Integer -> Bool

foreign import "primIntegerLeC" primIntegerLe :: Integer -> Integer -> Bool

