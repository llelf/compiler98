module Prelude(cHClose,primHClose,Handle) where

import IO
import NHC.Internal (IO(..))

cHClose primitive 1 :: Handle -> ()

primHClose                :: Handle -> IO () 
primHClose h = 
  IO ( \ world -> seq (cHClose h) (Right ()) )
