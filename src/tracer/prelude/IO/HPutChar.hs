module IO where

import DIO
import PreludeBuiltin(Handle)
import LowIO(primHPutChar)

hPutChar              :: Handle -> Char -> IO ()
hPutChar h chr         = primHPutChar h chr
