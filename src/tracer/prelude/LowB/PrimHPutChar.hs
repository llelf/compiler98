module Prelude(primHPutChar, cHPutChar) where

import PreludeBuiltin(Handle)
import DIO

primHPutChar :: Handle -> Char -> IO ()
primHPutChar handle c = IO (const (cHPutChar handle c))

cHPutChar handle ch = _prim _tprim_chPutChar handle ch

_tprim_chPutChar primitive 3 :: Trace -> R Handle -> R Char -> R (Either IOError ())

