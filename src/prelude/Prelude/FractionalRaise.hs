module Prelude where

infixr 8  ^^

(^^)		:: (Fractional a, Integral b) => a -> b -> a
#if !defined(TRACING)
x ^^ n		=  if n >= 0 then x^n else recip(x^(negate n))
#else
x ^^ n		=  if n >= (fromInteger 0) then x^n else recip(x^(negate n))
#endif

