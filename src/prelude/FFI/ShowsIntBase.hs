module FFI where

import Char (intToDigit)

showsIntBase :: Integral a => a -> a -> ShowS
showsIntBase base n r
  | n < 0   = error "Numeric.showsIntBase: can't show negative numbers"
  | otherwise =
      let (n',d) = quotRem n base
          r'     = intToDigit (fromIntegral d) : r
      in if n' == 0 then r' else showsIntBase base n' r'
