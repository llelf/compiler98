module Text(showLitChar) where

--import Char(isDigit)
--import AsciiTab

showLitChar 		   :: Char -> ShowS
{-
showLitChar c | c > '\DEL' && c < '\xa0' =  showChar '\\' . protectEsc isDigit (shows (fromEnum c))
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
-}
showLitChar c | c >= ' '   =  showChar c
{-
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : (snd (asciiTab!!fromEnum c)))

protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s
-}
