module Prelude(Integral(..)) where

--import PrimIntegerFromInt
--import PrimOps

instance Integral Int  where
    n `quot`    d   	= _prim _tprim_IntQuot n d
    n `rem`     d   	= _prim _tprim_IntRem n d
    n `quotRem` d 	= (n `quot` d, n `rem` d)

--    toInteger n 	= primIntegerFromInt n

_tprim_IntQuot primitive 3 :: Trace -> R Int -> R Int -> R Int
_tprim_IntRem primitive 3 :: Trace -> R Int -> R Int -> R Int
