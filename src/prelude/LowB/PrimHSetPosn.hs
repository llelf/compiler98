module Prelude where

import IO
import DIO
import PreludeBuiltin(HandlePosn)

cHSetPosn primitive 2 :: Handle -> HandlePosn -> Either IOError ()

primHSetPosn :: Handle -> HandlePosn -> IO ()
primHSetPosn h i = 
  IO ( \ world -> cHSetPosn h i )
