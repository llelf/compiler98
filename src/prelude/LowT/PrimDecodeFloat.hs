module Prelude where

primDecodeFloatC :: Float -> (Integer,Int)
primDecodeFloatC f = (primDecodeFloatMantissa f, primDecodeFloatExponent f)

foreign import primDecodeFloatMantissa :: Float -> Integer
foreign import primDecodeFloatExponent :: Float -> Int
