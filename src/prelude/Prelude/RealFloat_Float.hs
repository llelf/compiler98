module Prelude(RealFloat(..)) where

#if !defined(TRACING)
import PrimDecodeFloat
import PrimEncodeFloat
import CRealFloat
#endif

-- WARNING 32bit IEEE float

instance  RealFloat Float  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 24         -- FLT_DIGITS
    floatRange _  = (negate 148,128) -- (FLT_MINEXP, FLT_MAXEXP)
#if !defined(TRACING)
    decodeFloat x = primDecodeFloat x
    encodeFloat x y = primEncodeFloat x y
#else
    decodeFloat x = _prim _tprim_DecodeFloat x
    encodeFloat x y = _prim _tprim_EncodeFloat x y
#endif

    isNaN x 	     = error "isNaN not implemented" -- TODO
    isInfinite x     = error "isInfinite not implemented" -- TODO
    isDenormalized x = error "isDenormalized not implemented" -- TODO
    isNegativeZero x = error "isNegativeZero not implemented" -- TODO

#if defined(TRACING)
_tprim_DecodeFloat primitive 2 :: Trace -> R Float -> R (Integer, Int)
_tprim_EncodeFloat primitive 3 :: Trace -> R Integer -> R Int -> R Float
#endif
