module Prelude(Real(..)) where

import Ratio((%))

instance  Real Double
#if !defined(TRACING)
                       where
    toRational x = case decodeFloat x of (m,n) -> (m%1)*(b%1)^^n
			where b     = floatRadix  x

#endif
