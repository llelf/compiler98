module Prelude where

#if !defined(TRACING)
_readConArg :: (Read a) => (String -> [(a->b,String)]) -> ReadS b
#else
_readConArg :: (Read a) => (String -> [(a->b,String)]) -> String -> [(b,String)]
#endif
_readConArg fun = \ r ->  [(c a,s) | 
		           (c,r) <- fun r,
		           (a,s) <- readsPrec 10 r]
