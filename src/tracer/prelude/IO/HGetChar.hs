module IO where

import DIO
import PreludeBuiltin(Handle)
import LowIO(primHGetChar)

hGetChar              :: Handle -> IO Char
hGetChar h             = primHGetChar h
