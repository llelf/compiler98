module Prelude where

import IO
import NHC.Internal (IO(..))

cHFileSize primitive 1 :: Handle -> (Either IOError Integer)

primHFileSize :: Handle -> IO Integer
primHFileSize h = IO ( \ world -> cHFileSize h )

