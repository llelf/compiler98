--  Wrapper that exports just the stuff normally defined in the 
--  standard module Char. 
--  Has to be transformed into TCharTracing.hs and renamed to TChar.hs.
--  The original name cannot be Char.hs because then nhc98
--  confuses it with the original Char.
module Char ( 
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,

-- ...and what the Prelude exports
    Char, String
    ) where

import PreludeBasic
