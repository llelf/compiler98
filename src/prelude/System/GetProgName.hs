module System where

import LowSystem(primGetProgName)

getProgName             :: IO String
getProgName = primGetProgName
