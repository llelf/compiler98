module Prelude where

import _ReadCon

#if !defined(TRACING)
_readCon0 :: Bool -> a -> String -> ReadS a
#else
_readCon0 :: Bool -> a -> String -> String -> [(a,String)]
#endif
_readCon0 b con str = 
    readParen b (_readCon con str)
