module Prelude where

reverse		:: [a] -> [a]
#if !defined(TRACING)
reverse		= foldl (flip (:)) [] 
#else
reverse	xs	= foldl (flip (:)) [] xs
#endif
