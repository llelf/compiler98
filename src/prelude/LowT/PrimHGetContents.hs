module Prelude where

import PreludeBuiltin(Handle)
import DIO
import PrimHGetChar

primHGetContents :: Handle -> IO [Char]
primHGetContents h =
  IO ( \ world -> Right (input h) )
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 [] -- Maybe an eof here?
               else
               	 toEnum c : input h

