module Prelude(RealFloat(..)) where

--import PrimDecodeFloat
--import PrimEncodeFloat

-- WARNING 32bit IEEE float

instance  RealFloat Float  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 24         -- FLT_DIGITS
    floatRange _  = (-148,128) -- (FLT_MINEXP, FLT_MAXEXP)
    decodeFloat x = _prim _tprim_DecodeFloat x -- primDecodeFloat x
    encodeFloat x y = _prim _tprim_EncodeFloat x y -- primEncodeFloat x y

    isNaN x 	     = error "isNaN not implemented" -- TODO
    isInfinity x     = error "isInfinity not implemented" -- TODO
    isDenormalized x = error "isDenormalized not implemented" -- TODO
    isNegativeZero x = error "isNegativeZero not implemented" -- TODO

_tprim_DecodeFloat primitive 2 :: Trace -> R Float -> R (Integer, Int)
_tprim_EncodeFloat primitive 3 :: Trace -> R Integer -> R Int -> R Float
