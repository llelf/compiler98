module Prelude where

primDecodeDoubleC   :: Double -> (Integer,Int)
primDecodeDoubleC d = (primDecodeDoubleMantissa d, primDecodeDoubleExponent d)

foreign import primDecodeDoubleMantissa :: Double -> Integer
foreign import primDecodeDoubleExponent :: Double -> Int

{-
foreign import primDecodeDoubleC :: Double -> (Integer,Int)
-}
