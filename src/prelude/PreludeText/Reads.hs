module Prelude where

#if !defined(TRACING)
reads 	        :: (Read a) => ReadS a
#else
reads 	        :: (Read a) => String -> [(a,String)]
#endif
reads		=  readsPrec 0
