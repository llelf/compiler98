module Prelude where

import IO
import PreludeBuiltin

-- Use _hGetChar instead, in strict context !

cHGetChar :: Handle -> Int
cHGetChar h = _hGetChar h


