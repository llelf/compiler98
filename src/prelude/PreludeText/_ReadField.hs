module Prelude where

import Char (isAlpha)
import NonStdTrace

#if !defined(TRACING)
_readField :: (Read a) => String -> String -> (String -> [(a->b,String)]) -> ReadS b
#else
_readField :: (Read a) => String -> String -> (String -> [(a->b,String)]) -> String -> [(b,String)]
#endif
_readField prefix name fun
    | isAlpha (head name) =				-- ordinary fieldname
        \ r ->  [(c a,s) | (c,r) <- fun r,
		           (tok,r) <- lex r,
                           tok == prefix,
		           (tok,r) <- lex r,
                           tok == name,
		           (tok,r) <- lex r,
                           tok == "=",
		           (a,s) <- readsPrec 10 r]
    | otherwise =					-- symbol fieldname
        \ r ->  [(c a,s) | (c,r) <- fun r,
		           (tok,r) <- lex r,
                           tok == prefix,
		           (tok,r) <- lex r,
                           tok == "(",
		           (tok,r) <- lex r,
                           tok == init (tail name),	-- trim parens off
		           (tok,r) <- lex r,
                           tok == ")",
		           (tok,r) <- lex r,
                           tok == "=",
		           (a,s) <- readsPrec 10 r]

_readFinal postfix reader =
        \ r ->  [(c,s) | (c,r) <- reader r,
                         (tok,s) <- lex r,
                         tok == postfix ]

