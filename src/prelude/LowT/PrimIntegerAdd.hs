module Prelude where

foreign import ccall "primIntegerAddC" primIntegerAdd :: Integer -> Integer -> Integer
