module Numeric where

import IsDigit
import DigitToInt
import ReadInt

#if !defined(TRACING)
readDec :: (Integral a) => ReadS a
#else
readDec :: (Integral a) => String -> [(a,String)]
#endif
readDec = readInt 10 isDigit digitToInt

