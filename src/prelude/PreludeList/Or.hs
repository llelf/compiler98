module Prelude where

or		:: [Bool] -> Bool 
#if !defined(TRACING)
or		= foldr (||) False
#else
or x		= foldr (||) False x
#endif
