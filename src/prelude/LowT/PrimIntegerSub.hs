module Prelude where

foreign import ccall "primIntegerSubC" primIntegerSub :: Integer -> Integer -> Integer
