module IO where

import IO
import LowIO(primHClose)

hClose                :: Handle -> IO () 
hClose h               = primHClose h
