module Prelude where

import IO
import NHC.Internal (IO(..))

cHSetBuffering primitive 2 :: Handle -> BufferMode -> Either IOError ()

primHSetBuffering :: Handle -> BufferMode -> IO ()
primHSetBuffering h bm = 
  IO ( \ world -> cHSetBuffering h bm )
