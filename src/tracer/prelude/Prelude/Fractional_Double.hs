module Prelude(Fractional(..)) where

import Ratio((%),numerator,denominator)

instance  Fractional Double  where
  x / y =  _prim _tprim_FractionalDoubleDivide x y -- x / y 		-- MAGIC
{-
  fromRational x = x'
        	where x'    = f e
	              f e   = if e' == e then y else f e'
        	              where y :: Double
                                    y      = encodeDouble (round (x * (1 % b) ^^ e)) e
                	            (_,e') = decodeDouble y
	              (_,e) = decodeDouble (fromInteger (numerator x) `asTypeOf` x'
        	                                / fromInteger (denominator x))
	              b     = DoubleRadix x'
-}

_tprim_FractionalDoubleDivide primitive 3 :: Trace -> R Double -> R Double -> R Double
