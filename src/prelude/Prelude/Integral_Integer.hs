module Prelude(Integral(..)) where

import PrimIntegerQuotRem

instance Integral Integer  where
    n `quot` d 	  = fst (primIntegerQuotRem n d)
    n `rem`  d    = snd (primIntegerQuotRem n d) 
    n `div`  d    = fst (divMod n d) 
    n `mod`  d 	  = snd (divMod n d) 

    n `quotRem` d = primIntegerQuotRem n d 

    toInteger n   = n 
