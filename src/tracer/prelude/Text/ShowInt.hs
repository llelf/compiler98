module Text where

showInt	:: (Integral a) => a -> ShowS
showInt i = showString "<showInt>" -- shows i -- error "showInt: not implemented"
{-
showInt n r =
	      let (n',d) = quotRem n 10
		  r' :: [Char]
		  r' = toEnum (fromEnum '0' + fromIntegral d) : r
	      in if n' == 0 then r' else showInt n' r'
-}
