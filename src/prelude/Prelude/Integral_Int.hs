module Prelude(Integral(..)) where

#if !defined(TRACING)
import PrimIntegerFromInt

instance Integral Int  where
    n `quot`    d   	= n `quot` d			-- MAGIC 
    n `rem`     d   	= n `rem`  d			-- MAGIC
    n `quotRem` d 	= (n `quot` d, n `rem` d) 	-- er, not so MAGIC!

    toInteger n 	= primIntegerFromInt n

#else

instance Integral Int  where
    n `quot`    d   	= _prim _tprim_IntQuot n d
    n `rem`     d   	= _prim _tprim_IntRem n d
    n `quotRem` d 	= (n `quot` d, n `rem` d)

--    toInteger n 	= primIntegerFromInt n

_tprim_IntQuot primitive 3 :: Trace -> R Int -> R Int -> R Int
_tprim_IntRem primitive 3 :: Trace -> R Int -> R Int -> R Int
#endif
