module Prelude where

zip	:: [a] -> [b] -> [(a,b)] 
#if !defined(TRACING)
zip	= zipWith (,)
#else
zip x y	= zipWith (,) x y
#endif
