module Numeric where

import IsOctDigit
import DigitToInt
import ReadInt

#if !defined(TRACING)
readOct :: (Integral a) => ReadS a
#else
readOct :: (Integral a) => String -> [(a,String)]
#endif
readOct = readInt  8 isOctDigit digitToInt

