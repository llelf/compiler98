module Char where

import IsDigit
import LexDigits

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
