module Prelude(primHGetChar, cHGetChar) where

import DIO
import DIOError
import PreludeBuiltin(Handle)

primHGetChar :: Handle -> IO Char
primHGetChar h = IO ( \ world -> input h )
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 Left (EOFError "hGetChar" h)
               else
               	 Right (toEnum c)

cHGetChar handle = _prim _tprim_chGetChar handle

_tprim_chGetChar primitive 2 :: Trace -> R Handle -> R Int
