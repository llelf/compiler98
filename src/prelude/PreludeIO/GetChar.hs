module Prelude where

import IO

getChar :: IO Char
getChar =  hGetChar stdin
