module Prelude where

import PreludeBuiltin
import DIO
-- import CHPutChar

primHPutChar :: Handle -> Char -> IO ()
primHPutChar handle c = IO ( \ world -> _hPutChar handle c)
