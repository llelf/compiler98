module IO where

import IO
import LowIO(primHPutChar)

hPutChar              :: Handle -> Char -> IO ()
hPutChar h chr         = primHPutChar h chr
