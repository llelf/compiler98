module Prelude where

import IO
import NHC.Internal (IO(..))
import DIOError
import PreludeBuiltin
import CHGetChar

primHGetChar :: Handle -> IO Char
primHGetChar h = IO ( \ world -> input h )
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 Left (IOErrorEOF h "hGetChar")
               else
               	 Right (toEnum c)

