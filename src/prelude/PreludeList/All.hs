module Prelude where

all		:: (a -> Bool) -> [a] -> Bool 
#if !defined(TRACING)
all p  		= and . map p
#else
all p  x	= (and . map p) x
#endif
