module Prelude(RealFloat(..)) where

--import PrimDecodeDouble
--import PrimEncodeDouble


-- WARNING 64bit IEEE float

instance  RealFloat Double  where
    floatRadix _  = 2          -- FLT_RADIX
    floatDigits _ = 53         -- DBL_DIGITS
    floatRange _  = (-1073,1024) -- (DBL_MINEXP, DBL_MAXEXP)
    decodeFloat x = _prim _tprim_DecodeDouble x -- primDecodeDouble x
    encodeFloat x y = _prim _tprim_EncodeDouble x y -- primEncodeDouble x y

    isNaN x 	     = error "isNaN not implemented" -- TODO
    isInfinity x     = error "isInfinity not implemented" -- TODO
    isDenormalized x = error "isDenormalized not implemented" -- TODO
    isNegativeZero x = error "isNegativeZero not implemented" -- TODO

_tprim_DecodeDouble primitive 2 :: Trace -> R Double -> R (Integer, Int)
_tprim_EncodeDouble primitive 3 :: Trace -> R Integer -> R Int -> R Double
