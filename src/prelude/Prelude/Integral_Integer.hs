module Prelude(Integral(..)) where

#if !defined(TRACING)
import PrimIntegerQuotRem

instance Integral Integer  where
    n `quot` d 	  = fst (primIntegerQuotRem n d)
    n `rem`  d    = snd (primIntegerQuotRem n d) 
    n `div`  d    = fst (divMod n d) 
    n `mod`  d 	  = snd (divMod n d) 

    n `quotRem` d = primIntegerQuotRem n d 

    toInteger n   = n 

#else

instance Integral Integer  where
    n `quot` d 	  = fst (_prim_IntegerQuotRem n d)
    n `rem`  d    = snd (_prim_IntegerQuotRem n d) 
    n `div`  d    = fst (divMod n d) 
    n `mod`  d 	  = snd (divMod n d) 

    n `quotRem` d = _prim _tprim_IntegerQuotRem n d 

    toInteger n   = n 

_prim_IntegerQuotRem a b = _prim _tprim_IntegerQuotRem a b
_tprim_IntegerQuotRem primitive 3 :: Trace -> R Integer -> R Integer -> R (Integer, Integer)

#endif
