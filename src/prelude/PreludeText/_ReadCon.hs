module Prelude where

#if !defined(TRACING)
_readCon :: a -> String -> ReadS a
#else
_readCon :: a -> String -> String -> [(a,String)]
#endif
_readCon con str = 
  	(\ r -> [(con,s) | 
		 (tok,s) <- lex r ,tok == str])
