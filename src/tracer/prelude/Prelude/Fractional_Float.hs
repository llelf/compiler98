module Prelude(Fractional(..)) where

import Ratio((%),numerator,denominator)

instance  Fractional Float  where
  x / y = _prim _tprim_FractionalFloatDivide x y -- x / y 		-- MAGIC
{-
  fromRational x = x'
        	where x'    = f e
	              f e   = if e' == e then y else f e'
        	              where y :: Float
                                    y      = encodeFloat (round (x * (1 % b) ^^ e)) e
                	            (_,e') = decodeFloat y
	              (_,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
        	                                / fromInteger (denominator x))
	              b     = floatRadix x'
-}

_tprim_FractionalFloatDivide primitive 3 :: Trace -> R Float -> R Float -> R Float
