module Prelude where

any		:: (a -> Bool) -> [a] -> Bool 
#if !defined(TRACING)
any p		= or . map p
#else
any p x		= (or . map p) x
#endif
