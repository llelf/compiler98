module Prelude where

primDecodeDoubleC   :: Double -> (Integer,Int)
primDecodeDoubleC d = (primDecodeDoubleMantissa d, primDecodeDoubleExponent d)

foreign import ccall primDecodeDoubleMantissa :: Double -> Integer
foreign import ccall primDecodeDoubleExponent :: Double -> Int
