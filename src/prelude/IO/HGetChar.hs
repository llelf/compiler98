module IO where

import IO
import LowIO(primHGetChar)

hGetChar              :: Handle -> IO Char
hGetChar h             = primHGetChar h
