module Prelude where

import IO
import NHC.Internal (IO(..))

cHSeek primitive 3 :: Handle -> SeekMode -> Int -> Either IOError ()

primHSeek :: Handle -> SeekMode -> Integer -> IO ()
primHSeek h sm i = 
  IO ( \ world -> cHSeek h sm (fromEnum i) )
