module Prelude where

import PreludeBuiltin
import NHC.Internal (IO(..))
-- import CHPutChar

primHPutChar :: Handle -> Char -> IO ()
primHPutChar handle c = IO ( \ world -> _hPutChar handle c)
