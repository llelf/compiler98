module Prelude(RealFloat(..)) where

import PrimDecodeDouble
import PrimEncodeDouble
import CRealFloat
#if defined(TRACING)
#endif


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
#if 0
    decodeFloat x = _prim _tprim_DecodeDouble x -- primDecodeDouble x
    encodeFloat x y = _prim _tprim_EncodeDouble x y -- primEncodeDouble x y
#endif
#endif

    isNaN x 	     = error "isNaN not implemented" -- TODO
    isInfinite x     = error "isInfinite not implemented" -- TODO
    isDenormalized x = error "isDenormalized not implemented" -- TODO
    isNegativeZero x = error "isNegativeZero not implemented" -- TODO


#if 0
_tprim_DecodeDouble primitive 2 :: Trace -> R Double -> R (Integer, Int)
_tprim_EncodeDouble primitive 3 :: Trace -> R Integer -> R Int -> R Double
#endif
