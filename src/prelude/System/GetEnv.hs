module System where

import LowSystem(primGetEnv)

getEnv                  :: String -> IO String
getEnv str = primGetEnv str
