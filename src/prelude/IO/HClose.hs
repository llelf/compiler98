module IO where

import DIO
import PreludeBuiltin(Handle)
import LowIO(primHClose)

hClose                :: Handle -> IO () 
hClose h               = primHClose h
