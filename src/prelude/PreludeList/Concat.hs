module Prelude where

concat          :: [[a]] -> [a]
#if !defined(TRACING)
concat           =  foldr (++) []
#else
concat x         =  foldr (++) [] x
#endif
