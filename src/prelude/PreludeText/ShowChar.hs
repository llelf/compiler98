module Prelude where

showChar	:: Char -> ShowS
#if !defined(TRACING)
showChar        = (:)
#else
showChar x xs	= x:xs  -- saturate trusted constructor
#endif
