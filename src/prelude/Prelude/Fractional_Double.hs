module Prelude (Fractional(..)) where

#if !defined(TRACING)
import Numerator
import Denominator
import RatioCon
import Num_Ratio
import RealFrac_Ratio

instance  Fractional Double  where
  x / y = x / y 		-- MAGIC
  fromRational x = x'
        	where x'    = f e
	              f e   = if e' == e then y else f e'
        	              where y :: Double
                                    y      = encodeFloat (round (x * (1 % b) ^^ e)) e
                	            (_,e') = decodeFloat y
	              (_,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
        	                                / fromInteger (denominator x))
	              b     = floatRadix x'

#else

import Ratio((%),numerator,denominator)

instance  Fractional Double  where
  x / y =  _prim _tprim_FractionalDoubleDivide x y

_tprim_FractionalDoubleDivide primitive 3 :: Trace -> R Double -> R Double -> R Double

#endif
