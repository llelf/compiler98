module Prelude where

import IO
import DIOError
import DIO

cHIsEOF  primitive 1 :: Handle -> Either IOError Bool

primHIsEOF :: Handle -> IO Bool
primHIsEOF h = IO ( \ world -> cHIsEOF h )

