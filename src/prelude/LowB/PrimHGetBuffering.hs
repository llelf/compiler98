module Prelude where

import IO
import DIO

cHGetBuffering primitive 1 :: Handle  -> Either IOError BufferMode

primHGetBuffering :: Handle -> IO BufferMode
primHGetBuffering h  = 
  IO ( \ world -> cHGetBuffering h )


