module Prelude where

import IO
import NHC.Internal (IO(..))
import PreludeBuiltin(HandlePosn)

cHGetPosn primitive 1 :: Handle -> Either IOError HandlePosn

primHGetPosn :: Handle -> IO HandlePosn
primHGetPosn h  = 
  IO ( \ world -> cHGetPosn h )
