module Prelude where

odd :: (Integral a) => a -> Bool
#if !defined(TRACING)
odd = not . even
#else
odd x = (not . even) x
#endif
