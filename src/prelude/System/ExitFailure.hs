module System where

import System
import ExitWith

exitFailure     :: IO a
exitFailure      = exitWith (exitFailure 255)	-- value 255 is arbitrary

