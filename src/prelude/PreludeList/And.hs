module Prelude where

and		:: [Bool] -> Bool 
#if !defined(TRACING)
and 		= foldr (&&) True
#else
and x		= foldr (&&) True x
#endif
