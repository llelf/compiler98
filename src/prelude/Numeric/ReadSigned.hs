module Numeric where

#if !defined(TRACING)
readSigned:: (Real a) => ReadS a -> ReadS a
#else
readSigned:: (Real a) => (String->[(a,String)]) -> String -> [(a,String)]
#endif
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(negate x,t) | ("-",s) <- lex r,
						      (x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
		      				(n,"")  <- readPos str]
