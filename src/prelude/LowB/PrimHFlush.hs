module Prelude where

import IO
import NHC.Internal (IO(..))

cHFlush primitive 1 :: Handle  -> Either IOError ()

primHFlush :: Handle -> IO ()
primHFlush h  = 
  IO ( \ world -> cHFlush h )
