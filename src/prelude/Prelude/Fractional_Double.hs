module Prelude (Fractional(..)) where

import Numerator
import Denominator
import RatioCon
import Num_Ratio
import RealFrac_Ratio
#if defined(TRACING)
import PrimsDouble
#endif

instance  Fractional Double  where
#if !defined(TRACING)
  x / y = x / y 		-- MAGIC
#else
  x / y = primDoubleDiv x y
#endif

  fromRational x =
      let f ex = let y :: Double
                     y  = encodeFloat (round (x * (1 % bd) ^^ ex)) ex
                     e' = snd (decodeFloat y)
                     bd = floatRadix x'
                 in if e' == ex then y else f e'
          e    = snd (decodeFloat (fromInteger (numerator x) `asTypeOf` x'
        	                                / fromInteger (denominator x)))
          x'   = f e
      in x'

#if 0
  fromRational x = x'
        	where x'    = f e
	              f e   = if e' == e then y else f e'
        	              where y :: Double
                                    y      = encodeFloat (round (x * (1 % b) ^^ e)) e
                	            (_,e') = decodeFloat y
	              (_,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
        	                                / fromInteger (denominator x))
	              b     = floatRadix x'
#endif


#if 0
import Ratio((%),numerator,denominator)

instance  Fractional Double  where
  x / y =  _prim _tprim_FractionalDoubleDivide x y

_tprim_FractionalDoubleDivide primitive 3 :: Trace -> R Double -> R Double -> R Double
#endif
