module Prelude where

primDecodeFloatC :: Float -> (Integer,Int)
primDecodeFloatC f = (primDecodeFloatMantissa f, primDecodeFloatExponent f)

foreign import ccall primDecodeFloatMantissa :: Float -> Integer
foreign import ccall primDecodeFloatExponent :: Float -> Int
