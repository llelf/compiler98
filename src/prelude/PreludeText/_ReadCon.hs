module Prelude where

_readCon :: a -> String -> ReadS a
_readCon con str = 
  	(\ r -> [(con,s) | 
		 (tok,s) <- lex r ,tok == str])
