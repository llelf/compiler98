module Prelude(cHClose,primHClose,Handle) where

import IO
import DIO

cHClose primitive 1 :: Handle -> ()

primHClose                :: Handle -> IO () 
primHClose h = 
  IO ( \ world -> seq (cHClose h) (Right ()) )
