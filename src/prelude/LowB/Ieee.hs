module Prelude where

foreign import "isnan" dIsNaN :: Double -> Bool
foreign import "isinf" dIsInf :: Double -> Bool

foreign cast floatToDouble :: Float -> Double
foreign cast doubleToFloat :: Double -> Float
