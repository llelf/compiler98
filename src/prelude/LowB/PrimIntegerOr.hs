module Prelude where

-- primIntegerOr primitive 2 :: Integer -> Integer -> Integer

foreign import "primintegerOrC" primIntegerOr :: Integer -> Integer -> Integer

