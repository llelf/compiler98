module IO where

import IO
import LowIO(primHFlush)

hFlush                :: Handle -> IO () 
hFlush h               = primHFlush h
