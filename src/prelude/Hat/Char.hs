module Char
    ( isAlpha
    , isAlphaNum
    , isAscii
    , isControl
    , isDigit
    , isHexDigit
    , isLower
    , isLatin1
    , isOctDigit
    , isPrint
    , isSpace
    , isUpper
    , toLower
    , toUpper
    , ord
    , chr
    , digitToInt
    , intToDigit
    , readLitChar
    , showLitChar
    , lexLitChar
    ) where

import Numeric (readDec,readOct,readHex,lexDigits)

asciiTab :: [(Char,String)]
asciiTab =
	[('\NUL',"NUL")
	,('\SOH',"SOH")
	,('\STX',"STX")
	,('\ETX',"ETX")
	,('\EOT',"EOT")
	,('\ENQ',"ENQ")
	,('\ACK',"ACK")
	,('\BEL',"BEL")
	,('\BS',"BS")
	,('\HT',"HT")
	,('\LF',"LF")
	,('\VT',"VT")
	,('\FF',"FF")
	,('\CR',"CR")
	,('\SO',"SO")
	,('\SI',"SI")
	,('\DLE',"DLE")
	,('\DC1',"DC1")
	,('\DC2',"DC2")
	,('\DC3',"DC3")
	,('\DC4',"DC4")
	,('\NAK',"NAK")
	,('\SYN',"SYN")
	,('\ETB',"ETB")
	,('\CAN',"CAN")
	,('\EM',"EM")
	,('\SUB',"SUB")
	,('\ESC',"ESC")
	,('\FS',"FS")
	,('\GS',"GS")
	,('\RS',"RS")
	,('\US',"US")
	,('\SP',"SP")
	]

chr   :: Int -> Char
chr    =  toEnum

digitToInt     :: Char -> Int
digitToInt c
  | isDigit c             = ord c - ord '0'
  | c >= 'a' && c <= 'f'  = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F'  = ord c - ord 'A' + 10
  | otherwise             = error "Char.digitToInt: not a digit"

intToDigit     :: Int -> Char
intToDigit i
  | i >= 0 && i <= 9    = chr (ord '0' + i)
  | i >= 10 && i <= 15  = chr (ord 'a' + i - 10)
  | otherwise           = error "Char.intToDigit: not a digit"

isAlpha		:: Char -> Bool
isAlpha c       =  isUpper c  ||  isLower c

isAlphaNum	   :: Char -> Bool
isAlphaNum c       = isAlpha c  ||  isDigit c 

isAscii		:: Char -> Bool
isAscii c  	=  fromEnum c < 128

isControl	:: Char -> Bool
isControl c  	= c < ' ' ||  (c >= '\DEL' && c <= '\x9f')
 
isDigit		:: Char -> Bool
isDigit c       = c >= '0'   &&  c <= '9' 

isHexDigit		:: Char -> Bool
isHexDigit c   = (c >= '0' && c <= '9')
              || (c >= 'A' && c <= 'F')
              || (c >= 'a' && c <= 'f')

isLatin1	:: Char -> Bool
isLatin1 c  	=  c < '\xFF'

-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
isLower		:: Char -> Bool
isLower c               =  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'

isOctDigit		:: Char -> Bool
isOctDigit c       = c >= '0'   &&  c <= '7' 

isPrint		:: Char -> Bool
isPrint c  	=  not (isControl c)

isSpace	   	:: Char -> Bool 
isSpace c       =  c `elem` " \t\n\r\f\v\xa0"

-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper		:: Char -> Bool
isUpper c  	=  c >= 'A' && c <= 'Z' 
		|| c >= '\xC0' && c <= '\xD6' 
                || c >= '\xD8' && c <= '\xDE'

-- Code in comments are from Haskell 1.2
lexLitChar		:: ReadS String
lexLitChar ('\\':s)	=  [('\\':esc, t) | (esc,t) <- lexEsc s]
	where
	lexEsc (c:s)	 | c `elem` "abfnrtv\\\"'" = [([c],s)]
	lexEsc s@(d:_)	 | isDigit d		 = lexDigits s
  	lexEsc ('^':c:s) | c >= '@' && c <= '_'  = [(['^',c],s)]
{-
	lexEsc ('o':s)	=  [('o':os, t) | (os,t) <- nonnull isOctDigit s]
	lexEsc ('x':s)	=  [('x':xs, t) | (xs,t) <- nonnull isHexDigit s]
	lexEsc s@(c:_)	 | isUpper c
			=  case [(mne,s') | mne <- "DEL":asciiTab,
					    ([],s') <- [match mne s]	  ]
			   of (pr:_) -> [pr]
			      []     -> []
-}
	lexEsc _	=  []
lexLitChar (c:s)	=  [([c],s)]
lexLitChar ""		= []

ord   :: Char -> Int
ord    =  fromEnum

readLitChar 		:: ReadS Char
readLitChar ('\\':s)	=  readEsc s
	where
	readEsc ('a':s)	 = [('\a',s)]
	readEsc ('b':s)	 = [('\b',s)]
	readEsc ('f':s)	 = [('\f',s)]
	readEsc ('n':s)	 = [('\n',s)]
	readEsc ('r':s)	 = [('\r',s)]
	readEsc ('t':s)	 = [('\t',s)]
	readEsc ('v':s)	 = [('\v',s)]
	readEsc ('\\':s) = [('\\',s)]
	readEsc ('"':s)	 = [('"',s)]
	readEsc ('\'':s) = [('\'',s)]
	readEsc ('^':c:s) | c >= '@' && c <= '_'
			 = [(chr (ord c - ord '@'), s)]
	readEsc s@(d:_) | isDigit d
			 = [(chr n, t) | (n,t) <- readDec s]
	readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
	readEsc ('x':s)	 = [(chr n, t) | (n,t) <- readHex s]
	readEsc s@(c:_) | isUpper c
			 = let table = ('\DEL', "DEL") : asciiTab
			   in case [(c,s') | (c, mne) <- table,
					     ([],s') <- [match mne s]]
			      of (pr:_) -> [pr]
				 []	-> []
	readEsc _	 = []

        match :: (Eq a) => [a] -> [a] -> ([a],[a])
        match (x:xs) (y:ys) | x == y  =  match xs ys
        match xs     ys		      =  (xs,ys)
readLitChar (c:s)	=  [(c,s)]


showLitChar 		   :: Char -> ShowS
showLitChar c | c > '\DEL' && c < '\xa0' =  showChar '\\' .
                                            protectEsc isDigit (shows (ord c))
showLitChar c | c > '\xff' && fromEnum c <= 0xffff
                                         =  showChar '\\' .
                                            protectEsc isDigit (shows (ord c))
showLitChar c | fromEnum c > 0xffff      =  error ("character "++
                                                   show (fromEnum c)++
                                                   " out of range")
showLitChar '\DEL'	   =  showString "\\DEL"
showLitChar '\\'	   =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'	   =  showString "\\a"
showLitChar '\b'	   =  showString "\\b"
showLitChar '\f'	   =  showString "\\f"
showLitChar '\n'	   =  showString "\\n"
showLitChar '\r'	   =  showString "\\r"
showLitChar '\t'	   =  showString "\\t"
showLitChar '\v'	   =  showString "\\v"
showLitChar '\SO'	   =  protectEsc (== 'H') (showString "\\SO")
showLitChar c		   =  showString ('\\' : (snd (asciiTab!!ord c)))

protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s

toLower	:: Char -> Char
toLower c | isUpper c	=  toEnum ((fromEnum c - fromEnum 'A') + fromEnum 'a')
	  | otherwise	=  c

toUpper :: Char -> Char
toUpper c | isLower c	=  toEnum ((fromEnum c - fromEnum 'a') + fromEnum 'A')
	  | otherwise	=  c
