module Prelude(Integral(..)) where

import PrimIntegerQuotRem

instance Integral Integer  where
    n `quot` d 	  = primIntegerQuot n d
    n `rem`  d    = primIntegerRem n d
    n `div`  d    = fst (divMod n d) 
    n `mod`  d 	  = snd (divMod n d) 

    n `quotRem` d = primIntegerQuotRem n d

    toInteger n   = n 



#if 0
-- earlier version for tracing
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
