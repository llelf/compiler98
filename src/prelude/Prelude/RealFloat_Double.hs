module Prelude(RealFloat(..)) where

import PrimDecodeDouble
import PrimEncodeDouble
import CRealFloat


-- WARNING 64bit IEEE float

instance  RealFloat Double  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 53         -- DBL_DIGITS
    floatRange _  = (negate 1073,1024) -- (DBL_MINEXP, DBL_MAXEXP)
    decodeFloat x = primDecodeDouble x
    encodeFloat x y = primEncodeDouble x y

    isNaN x 	     = error "isNaN not implemented" -- TODO
    isInfinite x     = error "isInfinite not implemented" -- TODO
    isDenormalized x = error "isDenormalized not implemented" -- TODO
    isNegativeZero x = error "isNegativeZero not implemented" -- TODO

