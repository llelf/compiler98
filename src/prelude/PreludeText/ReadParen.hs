module Prelude where

#if !defined(TRACING)
readParen   	:: Bool -> ReadS a -> ReadS a
#else
readParen   	:: Bool -> (String->[(a,String)]) -> String -> [(a,String)]
#endif
readParen b g	=  if b then mandatory else optional
		   where optional r  = g r ++ mandatory r
			 mandatory r = [(x,u) | ("(",s) <- lex r,
						(x,t)   <- optional s,
						(")",u) <- lex t    ]
