module Numeric where

import IsHexDigit
import DigitToInt
import ReadInt

#if !defined(TRACING)
readHex :: (Integral a) => ReadS a
#else
readHex :: (Integral a) => String -> [(a,String)]
#endif
readHex  =  readInt 16 isHexDigit digitToInt
