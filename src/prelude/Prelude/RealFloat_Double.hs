module Prelude(RealFloat(..)) where

import PrimDecodeDouble
import PrimEncodeDouble
import CRealFloat


-- WARNING 64bit IEEE float

instance  RealFloat Double  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 53         -- DBL_DIGITS
    floatRange _  = (negate 1073,1024) -- (DBL_MINEXP, DBL_MAXEXP)
#if !defined(TRACING)
    decodeFloat x = primDecodeDouble x
    encodeFloat x y = primEncodeDouble x y
#else
    decodeFloat x = primDecodeDoubleC x
    encodeFloat x y = primEncodeDoubleC x y
#endif

    isNaN x 	     = isnan x /= 0
    isInfinite x     = isinf x /= 0

    -- TODO
    isDenormalized x = False
    isNegativeZero x = False
    isIEEE x         = False


foreign import "isinf" isinf :: Double -> Int
foreign import "isnan" isnan :: Double -> Int
