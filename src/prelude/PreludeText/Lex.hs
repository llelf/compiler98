module Prelude where

import Char(isSpace,isAlpha,isDigit,isAlphaNum)
import LexDigits
import LexLitChar

#if !defined(TRACING)
lex 	    		:: ReadS String
#else
lex 	    		:: String -> [(String,String)]
#endif

lex ""			= [("","")]
lex (c:s) | isSpace c	= lex (dropWhile isSpace s)
lex ('\'':s)		= [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
					       ch /= "'"		]
lex ('"':s)		= [('"':str, t)      | (str,t) <- lexString s]
			  where
			  lexString ('"':s) = [("\"",s)]
			  lexString s = [(ch++str, u)
						| (ch,t)  <- lexStrItem s,
						  (str,u) <- lexString t  ]

			  lexStrItem ('\\':'&':s) = [("\\&",s)]
			  lexStrItem ('\\':c:s) | isSpace c
			      = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
			  lexStrItem s		  = lexLitChar s

lex (c:s) | isSingle c	= [([c],s)]
	  | isSym c	= [(c:sym,t)	     | (sym,t) <- [span isSym s]]
	  | isAlpha c	= [(c:nam,t)	     | (nam,t) <- [span isIdChar s]]
	  | isDigit c	= [(c:ds++fe,t)	     | (ds,s)  <- [span isDigit s],
					       (fe,t)  <- lexFracExp s	   ]
	  | otherwise	= []	-- bad character
		where
		isSingle c  =  c `elem` ",;()[]{}_`"
		isSym c	    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
		isIdChar c  =  isAlphaNum c || c `elem` "_'"

		lexFracExp ('.':s) = [('.':ds++e,u) | (ds,t) <- lexDigits s,
						      (e,u)  <- lexExp t    ]
		lexFracExp s	   = [("",s)]

		lexExp (e:s) | e `elem` "eE"
			 = [(e:c:ds,u) | (c:t)	<- [s], c `elem` "+-",
						   (ds,u) <- lexDigits t] ++
			   [(e:ds,t)   | (ds,t)	<- lexDigits s]
		lexExp s = [("",s)]
