module IO where

import IO
import LowIO(primHIsEOF)

hIsEOF                :: Handle -> IO Bool
hIsEOF h               = primHIsEOF h
