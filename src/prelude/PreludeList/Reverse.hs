module Prelude where

reverse		:: [a] -> [a]
#if !defined(TRACING)
reverse		= foldl (flip (:)) [] 
#else
reverse	xs	= foldl (\xs x -> x:xs) [] xs
#endif
