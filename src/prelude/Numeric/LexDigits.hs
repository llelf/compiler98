module Numeric where

import IsDigit
import Nonnull

#if !defined(TRACING)
lexDigits		:: ReadS String	
#else
lexDigits		:: String -> [(String,String)]
#endif
lexDigits		=  nonnull isDigit
