module Prelude where

foreign import ccall "primIntegerFromIntC" primIntegerFromInt :: Int -> Integer
