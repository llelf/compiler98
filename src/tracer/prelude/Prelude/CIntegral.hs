module Prelude where

infixl 7  `quot`, `rem`, `div`, `mod`


class  (Real a, Enum a) => Integral a  where
    quot, rem, div, mod	:: a -> a -> a
    quotRem, divMod	:: a -> a -> (a,a)
    toInteger		:: a -> Integer

--    n `quot` d		=  fst (quotRem n d)
--    n `rem`  d		=  snd (quotRem n d)
    n `div`  d          =  fst (divMod  n d)
    n `mod`  d          =  snd (divMod  n d)

    n `quotRem` d	= (n `quot` d, n `rem` d)

    divMod n d          =  if signum r == - signum d then (q-1, r+d) else qr
			     where qr@(q,r) = quotRem n d 

