module Prelude where

import IO
import PreludeBuiltin

-- Use _hPutChar instead, in strict context !

cHPutChar :: Handle -> Char -> Either IOError ()
cHPutChar h c = _hPutChar h c

