module Prelude where

import _ReadCon

_readCon0 :: Bool -> a -> String -> ReadS a
_readCon0 b con str = 
    readParen b (_readCon con str)
