module Prelude(Fractional(..)) where

import Numerator
import Denominator
import RatioCon
import Num_Ratio
import RealFrac_Ratio

#if defined(TRACING)
import PrimsFloat
#endif

instance  Fractional Float  where
#if !defined(TRACING)
  x / y =  x / y 		-- MAGIC
#else
  x / y = primFloatDiv x y
#endif
  fromRational x = x0
      where x0    = ff ef
            ff ef = if ef' == ef then yf else ff ef'
                   where yf :: Float
                         yf      = encodeFloat (round (x * (1 % bf) ^^ ef)) ef
                         (_,ef') = decodeFloat yf
                         bf      = floatRadix x0
            (_,ef) = decodeFloat (fromInteger (numerator x) `asTypeOf` x0
        	                                / fromInteger (denominator x))

