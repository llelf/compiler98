module Numeric where

import Char (intToDigit)

showIntBase :: Integral a => a -> a -> ShowS
showIntBase base n r
  | n < 0   = error "Numeric.showIntBase: can't show negative numbers"
  | otherwise =
      let (n',d) = quotRem n base
          r'     = intToDigit (fromIntegral d) : r
      in if n' == 0 then r' else showIntBase base n' r'
