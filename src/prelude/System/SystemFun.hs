module System where

import System
import LowSystem(primSystem)

system :: String -> IO ExitCode
system = primSystem
